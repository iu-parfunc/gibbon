#![allow(dead_code)]

/*!

Implementation of the new generational garbage collector for Gibbon.

 */

use libc;

use std::collections::{HashMap, HashSet};
use std::error::Error;
// use std::intrinsics::ptr_offset_from;
use std::mem::size_of;
use std::ptr::{null, null_mut, write_bytes};

use crate::ffi::c::*;
use crate::record_time;
use crate::tagged_pointer::*;
use crate::{dbgprintln, worklist_next, write_shortcut_ptr};

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Garbage Collector; evacuation, promotion etc.
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

/// If we have variable sized initial chunks due to the region bound analysis,
/// we'll have to store that info in a chunk footer, and then that could guide
/// how big this new chunk should be. Using a default value of 1024 for now.
const CHUNK_SIZE: usize = 1024;
const MAX_CHUNK_SIZE: usize = 65500;

/// Heuristic for when to collect the old generation. Other possible heuristics:
/// when oldgen has X bytes or a region has Y depth.
const COLLECT_MAJOR_K: u8 = 4;

/// A mutable global to store various stats.
static mut GC_STATS: *mut GibGcStats = null_mut();

/// This stores pairs of (start_address -> end_address) of evacuated objects,
/// in case we need to skip over them during subsequent evacuation. We only
/// store these for objects starting at non-loc0 positions.
///
/// E.g. | A ... C 1 ... | if we evacuate 'C' first, we'll burn it and create a
/// hole in its place. If we subsequently want to evacuate the bigger object
/// starting at 'A', we would need to skip over 'C' to get to the next field.
type SkipoverEnv = HashMap<*mut i8, *mut i8>;

/// When evacuating a value made exclusively of non-forwardable objects and
/// located in the middle of a chunk, we need to store its forwarding address
/// separately in this table. We store pairs of (start_address -> fwd_address).
type ForwardingEnv = HashMap<*mut i8, *mut i8>;

/// Things needed during evacuation. This reduces the number of arguments needed
/// for the evacuation function, so that all of its arguments can be passed
/// in registers. This was much more important when evacuation was implemented
/// as a set of mutually recursive functions, but less so with the new worklist
/// algorithm.
#[derive(Debug)]
struct EvacState<'a> {
    so_env: &'a mut SkipoverEnv,
    zct: *mut Zct,
    nursery: &'a GibNursery,
    evac_major: bool,
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#[cfg(feature = "verbose_evac")]
#[macro_export]
macro_rules! dbgprintln {
    () => {
        print!("\n")
    };
    ($($arg:tt)*) => {{
        println!($($arg)*);

    }};
}

#[cfg(not(feature = "verbose_evac"))]
#[macro_export]
macro_rules! dbgprintln {
    () => {};
    ($($arg:tt)*) => {};
}

#[cfg(feature = "verbose_evac")]
#[macro_export]
macro_rules! dbgprint {
    () => {
        print!("\n")
    };
    ($($arg:tt)*) => {{
        print!($($arg)*);

    }};
}

#[cfg(not(feature = "verbose_evac"))]
#[macro_export]
macro_rules! dbgprint {
    () => {};
    ($($arg:tt)*) => {};
}

pub fn cleanup(
    rstack: &mut GibShadowstack,
    wstack: &mut GibShadowstack,
    nursery: &mut GibNursery,
    oldgen: &mut GibOldgen,
) -> Result<()> {
    unsafe {
        // Free the info table.
        _INFO_TABLE.drain(..);
        _INFO_TABLE.shrink_to_fit();
    }
    // Free all the regions.
    unsafe {
        for frame in rstack.into_iter().chain(wstack.into_iter()) {
            let footer = (*frame).endptr as *const GibOldgenChunkFooter;
            if !nursery.contains_addr((*frame).ptr) {
                (*((*oldgen).old_zct)).insert((*footer).reg_info);
            }
        }
        for reg_info in (*((*oldgen).old_zct)).drain() {
            free_region((*reg_info).first_chunk_footer, null_mut())?;
        }
    }
    // Free ZCTs associated with the oldest generation.
    oldgen.free_zcts();
    Ok(())
}

pub fn garbage_collect(
    rstack: &mut GibShadowstack,
    wstack: &mut GibShadowstack,
    nursery: &mut GibNursery,
    oldgen: &mut GibOldgen,
    gc_stats: *mut GibGcStats,
    force_major: bool,
) -> Result<()> {
    dbgprintln!("gc...");

    unsafe {
        // Start by cauterizing the writers.
        cauterize_writers(nursery, wstack)?;
        // let evac_major = force_major
        //     || (*nursery_ptr).num_collections.rem_euclid(COLLECT_MAJOR_K.into())
        //         == 0;
        let evac_major = force_major;

        // Set the global stats object pointer.
        GC_STATS = gc_stats;
        // Update stats.
        #[cfg(feature = "gcstats")]
        {
            (*GC_STATS).minor_collections += 1;
            if evac_major {
                (*GC_STATS).major_collections += 1;
            }
        }

        // Start collection.
        #[cfg(feature = "verbose_evac")]
        {
            rstack.print_all("Read");
            wstack.print_all("Write");
            (*oldgen).rem_set.print_all("Remembered set");
            // print_nursery_and_oldgen(rstack, wstack, nursery, oldgen);
        }

        // Evacuate readers.
        let mut so_env = HashMap::new();
        evacuate_shadowstack(
            &mut so_env,
            nursery,
            oldgen,
            rstack,
            evac_major,
        )?;

        // Collect dead regions.
        oldgen.collect_regions()?;

        // Reset the allocation area and record stats.
        nursery.clear();

        // Restore the remaining cauterized writers.
        restore_writers(wstack, nursery, oldgen)?;
        Ok(())
    }
}

/// Write a CAUTERIZED_TAG at all write cursors so that the collector knows
/// when to stop copying.
fn cauterize_writers(
    nursery: &GibNursery,
    wstack: &GibShadowstack,
) -> Result<()> {
    for frame in wstack.into_iter() {
        unsafe {
            if !nursery.contains_addr((*frame).ptr) {
                continue;
            }
            let ptr = (*frame).ptr;
            // Layout for cauterized object is a tag followed by a pointer back to the frame.:
            let ptr_next = write(ptr, CAUTERIZED_TAG);
            write(ptr_next, frame);
        }
    }
    Ok(())
}

/// See Note [Restoring uncauterized write cursors].
fn restore_writers(
    wstack: &GibShadowstack,
    nursery: &mut GibNursery,
    oldgen: &mut GibOldgen,
) -> Result<()> {
    let mut env: HashMap<*mut i8, (*mut i8, *mut i8)> = HashMap::new();
    for frame in wstack.into_iter() {
        unsafe {
            if nursery.contains_addr((*frame).ptr) {
                #[cfg(debug_assertions)]
                {
                    if !is_loc0((*frame).ptr, (*frame).endptr, true) {
                        panic!(
                            "Uncauterized write cursor and not loc0, {:?}",
                            *frame
                        );
                    }
                }

                match env.get(&(*frame).ptr) {
                    None => {
                        let (chunk_start, chunk_end) =
                            Heap::allocate_first_chunk(oldgen, CHUNK_SIZE, 0)?;
                        env.insert((*frame).ptr, (chunk_start, chunk_end));

                        dbgprintln!(
                            "+Restoring writer {:?} to {:?}",
                            (*frame).ptr,
                            chunk_start
                        );

                        (*frame).ptr = chunk_start;
                        (*frame).endptr = chunk_end;
                    }
                    Some((chunk_start, chunk_end)) => {
                        dbgprintln!(
                            "+Restoring writer {:?} to {:?}",
                            (*frame).ptr,
                            *chunk_start
                        );

                        (*frame).ptr = *chunk_start;
                        (*frame).endptr = *chunk_end;
                    }
                }
            }
        }
    }
    Ok(())
}

/// Copy values at all read cursors from the nursery to the provided
/// destination heap. Also uncauterize any writer cursors that are reached.
unsafe fn evacuate_shadowstack<'a, 'b>(
    so_env: &'a mut SkipoverEnv,
    nursery: &'b GibNursery,
    oldgen: &'a mut GibOldgen,
    rstack: &GibShadowstack,
    evac_major: bool,
) -> Result<()> {
    let rem_set: &&mut GibShadowstack = &(*oldgen).rem_set;
    let frames = record_time!(
        sort_roots(rstack, rem_set),
        (*GC_STATS).gc_rootset_sort_time
    );

    #[cfg(debug_assertions)]
    {
        for frame in frames.iter() {
            assert!((*(*frame)).ptr < (*(*frame)).endptr);
        }
    }

    #[cfg(feature = "verbose_evac")]
    {
        dbgprintln!("Sorted roots:");
        for frame in frames.iter() {
            dbgprintln!("{:?}", *(*frame));
        }
    }

    let mut st =
        EvacState { so_env, zct: (*oldgen).new_zct, nursery, evac_major };

    for frame in frames {
        dbgprintln!("+Evacuating root {:?}", (*frame));

        match (*frame).gc_root_prov {
            GibGcRootProv::RemSet => {
                let (tagged_src, _): (u64, _) = read((*frame).ptr);
                let tagged = TaggedPointer::from_u64(tagged_src);
                let src = tagged.untag();
                let src_footer_offset = tagged.get_tag();
                // let src_end = src.add(src_footer_offset as usize);
                let (tag, src_after_tag): (GibPackedTag, *mut i8) =
                    read_mut(src);
                match tag {
                    // If the data is already evacuated, don't allocate a fresh
                    // destination region. Just update the root to point to the
                    // evacuated data.
                    COPIED_TAG | COPIED_TO_TAG => {
                        evacuate_copied(frame, tag, src, src_after_tag);
                    }
                    _ => {
                        // Allocate space in the destination.
                        let (dst, dst_end) =
                            Heap::allocate_first_chunk(oldgen, CHUNK_SIZE, 1)?;
                        // Evacuate the data.
                        let (src_after, dst_after, dst_after_end, _forwarded) =
                            evacuate_packed(
                                &mut st, oldgen, frame, dst, dst_end,
                            );
                        // Update the indirection pointer in oldgen region.
                        let offset = dst_end.offset_from(dst);
                        let tagged: u64 =
                            TaggedPointer::new(dst, offset as u16).as_u64();

                        dbgprintln!(
                            "+Restoring reader {:?} to {:?}",
                            (*frame).ptr,
                            dst
                        );

                        write((*frame).ptr, tagged);

                        dbgprintln!(
                            "   wrote tagged indirection pointer {:?} -> ({:?},{:?})",
                            (*frame).ptr, dst, offset
                        );

                        // Update the outset in oldgen region.
                        //
                        // We don't need to update the refcount here because we've
                        // already created the new region with an initial refcount
                        // of 1 above.
                        add_to_outset((*frame).endptr, dst_end);

                        // Note [Adding a forwarding pointer at the end of every chunk].
                        // Compute chunk size.
                        let src_footer: *mut GibNurseryChunkFooter =
                            (*frame).endptr as *mut GibNurseryChunkFooter;
                        let chunk_size = *src_footer as usize;
                        let is_loc_0 =
                            (src_footer_offset as usize) == chunk_size;

                        // Write a forwarding pointer if we burned a hole in
                        // the middle of a buffer.
                        if is_loc_0 {
                            // Write forwarding pointer at the end of chunk.
                            debug_assert!(dst_after < dst_after_end);
                            write_forwarding_pointer_at(
                                src_after,
                                dst_after,
                                dst_after_end.offset_from(dst_after) as u16,
                            );
                        } else {
                            // No forwarding pointer can be written,
                            // add an entry o forwarding env.
                            // TODO:
                        }
                    }
                }
            }
            GibGcRootProv::Stk => {
                let root_in_nursery = nursery.contains_addr((*frame).ptr);
                if !root_in_nursery {
                    dbgprintln!(
                        "Evac packed, skipping oldgen root {:?}",
                        (*frame)
                    );

                    let _footer =
                        (*frame).endptr as *const GibOldgenChunkFooter;
                    /*
                    if (*((*footer).reg_info)).refcount == 0 {
                        (*zct).insert((*footer).reg_info);
                    }
                     */
                    continue;
                }

                let src = (*frame).ptr;
                let src_end = (*frame).endptr;
                let (tag, src_after_tag): (GibPackedTag, *mut i8) =
                    read_mut(src);

                match tag {
                    // If the data is already evacuated, don't allocate a fresh
                    // destination region. Just update the root to point to the
                    // evacuated data.
                    COPIED_TAG | COPIED_TO_TAG => {
                        evacuate_copied(frame, tag, src, src_after_tag);
                    }
                    _ => {
                        // We already know that this root is in the nursery.

                        // Compute chunk size.
                        let src_footer: *mut GibNurseryChunkFooter =
                            (*frame).endptr as *mut GibNurseryChunkFooter;
                        let chunk_size = *src_footer as usize;

                        // Allocate space in the destination.
                        let (dst, dst_end) =
                            Heap::allocate_first_chunk(oldgen, CHUNK_SIZE, 0)?;
                        // Update ZCT.
                        let footer = dst_end as *const GibOldgenChunkFooter;
                        record_time!(
                            (*(st.zct)).insert((*footer).reg_info),
                            (*GC_STATS).gc_zct_mgmt_time
                        );

                        // Evacuate the data.
                        let (src_after, dst_after, dst_after_end, forwarded) =
                            evacuate_packed(
                                &mut st, oldgen, frame, dst, dst_end,
                            );

                        dbgprintln!(
                            "+Restoring reader {:?} to {:?}",
                            (*frame).ptr,
                            dst
                        );

                        // Update the pointers in shadow-stack.
                        (*frame).ptr = dst;
                        // TODO: AUDITME.
                        // (*frame).endptr = dst_after_end;
                        (*frame).endptr = dst_end;

                        // Note [Adding a forwarding pointer at the end of every chunk].
                        let is_loc_0 =
                            (src_end.offset_from(src)) == chunk_size as isize;
                        if !forwarded {
                            if is_loc_0 {
                                // Write forwarding pointer at the end of chunk.
                                debug_assert!(dst_after < dst_after_end);
                                write_forwarding_pointer_at(
                                    src_after,
                                    dst_after,
                                    dst_after_end.offset_from(dst_after)
                                        as u16,
                                );
                            } else {
                                // No forwarding pointer can be written,
                                // add an entry o forwarding env.
                                // TODO:
                            }
                        }
                    }
                }
            }
        }
    }
    // Clear the remembered set.
    (*oldgen).rem_set.clear();
    Ok(())
}

#[inline(always)]
unsafe fn evacuate_copied(
    frame: *mut GibShadowstackFrame,
    tag: u8,
    src: *mut i8,
    src_after_tag: *mut i8,
) {
    dbgprintln!(
        "   optimization! Didn't need to allocate region for root {:?}",
        (*frame)
    );

    let tagged_fwd_ptr = record_time!(
        find_forwarding_pointer(tag, src, src_after_tag,),
        (*GC_STATS).gc_find_fwdptr_time
    );

    let fwd_ptr = tagged_fwd_ptr.untag();
    let fwd_footer_offset = tagged_fwd_ptr.get_tag();
    let fwd_footer_addr = fwd_ptr.add(fwd_footer_offset as usize);

    match (*frame).gc_root_prov {
        GibGcRootProv::Stk => {
            dbgprintln!(
                "+Restoring reader {:?} to {:?}",
                (*frame).ptr,
                fwd_ptr
            );
            (*frame).ptr = fwd_ptr;
            (*frame).endptr = fwd_footer_addr;
        }
        GibGcRootProv::RemSet => {
            dbgprintln!(
                "*Writing {:?} at {:?} in oldgen",
                fwd_ptr,
                (*frame).ptr
            );

            // Update the indirection pointer in oldgen region.
            write((*frame).ptr, fwd_ptr);

            // Update the outset in oldgen region.
            handle_old_to_old_indirection((*frame).endptr, fwd_footer_addr);
        }
    }
}

#[derive(Debug)]
// The actions pushed on the stack while evacuating.
enum EvacAction {
    // The datatype to process along with an optional shortcut pointer address
    // that needs to be updated to point to the new destination of this type.
    ProcessTy(GibDatatype, Option<*mut i8>),
    // A reified continuation of processing the target of an indirection.
    RestoreSrc(*mut i8),
    // Update the skip-over environment.
    SkipoverEnvWrite(*mut i8),
}

/*

[2022.09.06] Main todos:

(1)

Suppose there is an indirection pointer pointing to a an address ABC within the
nursery. However, the address ABC contains a redirection pointer. Under the usual
policy the GC will inline this indirection, which is BAD. Redirections stop
evacuation; they always point to an oldgen chunk and there's never any data
after them. But inlining a redirection breaks these assumptions. Since the GC
can very well write data after the inlined redirection. The following code
prevents this inlining. Instead, it'll make the GC write an indirection pointer
pointing to the target of the redirection.

Also, make indirection/redirection branches use the proper end-of-input-region
by using the tag.

[2022.11.30]: this is implemented but redirection pointers are still buggy.


(2)

Properly handle values that start in the nursery but end in the old generation,
possibly due to eager promotion.

*/

/**

Evacuate a packed value by referring to the info table.

FIXME: forwarded is currently for the entire (multi-chunk) evaluation, and it should be per-chunk.

 */
unsafe fn evacuate_packed(
    st: &mut EvacState,
    oldgen: &mut impl Heap,
    frame: *const GibShadowstackFrame,
    orig_dst: *mut i8,
    orig_dst_end: *mut i8,
) -> (*mut i8, *mut i8, *mut i8, bool) {
    dbgprintln!("Start evacuation {:?} -> {:?}", (*frame).ptr, orig_dst);

    let orig_typ = (*frame).datatype;
    let orig_src = match (*frame).gc_root_prov {
        GibGcRootProv::Stk => (*frame).ptr,
        GibGcRootProv::RemSet => {
            // The remembered set contains the address where the indirect-
            // ion pointer is stored. We must read it to get the address of
            // the pointed-to data.
            let (tagged_src, _): (u64, _) = read((*frame).ptr);
            TaggedPointer::from_u64(tagged_src).untag()
        }
    };

    // These comprise the main mutable state of the traversal and should be updated
    // together at the end of every iteration:
    let mut src = orig_src;
    let mut dst = orig_dst;
    let mut dst_end = orig_dst_end;
    let mut inlining_underway = false;
    // Address of the field after the indirection being inlined. When we reach a
    // RestoreSrc with this address, inlining_underway is set to false again.
    let mut inlining_underway_upto = null_mut();
    let mut burn = true;
    // The implicit -1th element of the worklist:
    let mut next_action = EvacAction::ProcessTy(orig_typ, None);
    let mut forwarded = false;

    dbgprintln!("Evac packed {:?} -> {:?}", src, dst);

    // Stores everything to process AFTER the next_action.
    let mut worklist: Vec<EvacAction> = Vec::new();

    loop {
        dbgprintln!("++Loop iteration on src {:?}, inlining_underway {:?}, action {:?}, length after this {}, prefix(5): {:?}",
                    src, inlining_underway, next_action, worklist.len(), &worklist[..std::cmp::min(5, worklist.len())]);

        match next_action {
            EvacAction::RestoreSrc(new_src) => {
                dbgprintln!("+++Restoring source from {:?} to {:?}, inlining_underway_upto={:?}",
                            src, new_src, inlining_underway_upto);

                src = new_src;

                if new_src == inlining_underway_upto {
                    inlining_underway = false;
                    burn = true;
                }

                worklist_next!(worklist, next_action);
            }
            EvacAction::ProcessTy(next_ty, mb_shortcut_addr) => {
                dbgprintln!(
                    "   shortcut pointer at {:?} will be updated",
                    mb_shortcut_addr
                );

                let (tag, src_after_tag): (GibPackedTag, *mut i8) =
                    read_mut(src);

                dbgprintln!("+++Read next tag {} from src {:?}", tag, src);

                match tag {
                    // A pointer to a value in another buffer; copy this value
                    // and then switch back to copying rest of the source buffer.
                    //
                    // POLICY DECISION:
                    // Indirections into oldgen are not inlined in the current version.
                    // See Note [Smart inlining policies].
                    INDIRECTION_TAG => {
                        let (tagged_pointee, src_after_indr): (u64, _) =
                            read(src_after_tag);

                        dbgprintln!(
                            "   indirection! src {:?} dest {:?}, after {:?}",
                            src_after_tag,
                            tagged_pointee as *mut i8,
                            src_after_indr
                        );

                        let src_after_indr1 = src_after_indr as *mut i8;
                        let tagged = TaggedPointer::from_u64(tagged_pointee);
                        let pointee = tagged.untag();
                        let pointee_footer_offset = tagged.get_tag();
                        let pointee_footer =
                            pointee.add(pointee_footer_offset as usize);

                        // Add a forwarding pointer in the source buffer.
                        debug_assert!(dst < dst_end);

                        if burn {
                            write_forwarding_pointer_at(
                                src,
                                dst,
                                dst_end.offset_from(dst) as u16,
                            );
                            forwarded = true;

                            #[cfg(feature = "gcstats")]
                            {
                                (*GC_STATS).forwarded += 1;
                            }
                        }

                        // If the pointee is in the nursery, evacuate it.
                        // Otherwise, write an indirection node at dst and adjust the
                        // refcount and outset.
                        if st.evac_major || st.nursery.contains_addr(pointee) {
                            dbgprintln!("   inlining indirection");

                            // TAIL OPTIMIZATION: if we're the last thing, in the worklist, don't bother restoring src:
                            if !worklist.is_empty() {
                                worklist.push(EvacAction::RestoreSrc(
                                    src_after_indr1,
                                ));
                            } else {
                                dbgprintln!("   tail optimization!");
                            }

                            if !inlining_underway {
                                inlining_underway = true;
                                inlining_underway_upto = src_after_indr1;
                            }

                            src = pointee;

                            #[cfg(feature = "gcstats")]
                            {
                                (*GC_STATS).indirs_inlined += 1;
                            }

                            // Update the burned environment if we're evacuating a root
                            // from the remembered set of if we're evacuating an
                            // indirection pointer that points to a non-zero location.
                            match (*frame).gc_root_prov {
                                GibGcRootProv::RemSet => {
                                    dbgprintln!(
                                        "   pushing SkipoverEnvWrite({:?}) action to stack for indir, root in remembered set",
                                        src,
                                    );
                                    worklist.push(
                                        EvacAction::SkipoverEnvWrite(pointee),
                                    );
                                }

                                GibGcRootProv::Stk => {
                                    if !is_loc0(pointee, pointee_footer, true)
                                    {
                                        dbgprintln!(
                                            "   pushing SkipoverEnvWrite({:?}) action to stack for indir, root in nursery",
                                            src,
                                        );

                                        worklist.push(
                                            EvacAction::SkipoverEnvWrite(
                                                pointee,
                                            ),
                                        );
                                    } else {
                                        dbgprintln!(
                                            "   optimization! did not push SkipoverEnvWrite({:?} to {:?}) to stack",
                                            pointee,
                                            src
                                        );
                                    }
                                }
                            }

                            // Same type, new location to evac from:
                            next_action = EvacAction::ProcessTy(
                                next_ty,
                                mb_shortcut_addr,
                            );
                            continue;
                        } else {
                            let space_reqd = 32;
                            let (dst1, dst_end1) = Heap::check_bounds(
                                oldgen, space_reqd, dst, dst_end,
                            );
                            let dst_after_tag = write(dst1, INDIRECTION_TAG);
                            let dst_after_indr =
                                write(dst_after_tag, tagged_pointee);
                            write_shortcut_ptr!(
                                mb_shortcut_addr,
                                dst1,
                                dst_end1
                            );

                            // Update the burned environment if we're evacuating a root
                            // from the remembered set of if we're evacuating an
                            // indirection pointer that points to a non-zero location.
                            match (*frame).gc_root_prov {
                                GibGcRootProv::RemSet => {
                                    dbgprintln!(
                                        "   inserting ({:?} to {:?}) to so_env, root in remembered set",
                                        src,
                                        src_after_indr1,
                                    );
                                    st.so_env.insert(src, src_after_indr1);
                                }

                                GibGcRootProv::Stk => {
                                    if !is_loc0(pointee, pointee_footer, true)
                                    {
                                        dbgprintln!(
                                            "   inserting ({:?} to {:?}) to so_env, root in nursery",
                                            src,
                                            src_after_indr1,
                                        );

                                        st.so_env.insert(src, src_after_indr1);
                                    } else {
                                        dbgprintln!(
                                            "   optimization! did not insert ({:?} to {:?}) to so_env",
                                            pointee,
                                            src
                                        );
                                    }
                                }
                            }

                            let pointee_footer_offset = tagged.get_tag();
                            let pointee_footer =
                                pointee.add(pointee_footer_offset as usize);
                            handle_old_to_old_indirection(
                                dst_end1,
                                pointee_footer,
                            );

                            (*(st.zct)).insert(
                                (*(pointee_footer
                                    as *mut GibOldgenChunkFooter))
                                    .reg_info
                                    as *const GibRegionInfo,
                            );

                            src = src_after_indr1;
                            dst = dst_after_indr;
                            dst_end = dst_end1;

                            dbgprintln!(
                                "   wrote tagged indirection pointer {:?} -> ({:p},{:?})",
                                dst_after_tag, tagged.untag() as *const i8, tagged.get_tag()
                            );

                            #[cfg(feature = "gcstats")]
                            {
                                (*GC_STATS).mem_copied += 9;
                                (*GC_STATS).indirs_not_inlined += 1;
                            }

                            worklist_next!(worklist, next_action);
                        }
                    }

                    // Nothing to copy. Just update the write cursor's new
                    // address in shadow-stack.
                    CAUTERIZED_TAG => {
                        // Recover the reverse-pointer back to the shadowstack frame:
                        let (wframe_ptr, after_wframe_ptr): (
                            *mut i8,
                            *mut i8,
                        ) = read_mut(src_after_tag);
                        let wframe = wframe_ptr as *mut GibShadowstackFrame;
                        // Update the pointers on the shadow-stack.
                        (*wframe).ptr = dst;
                        (*wframe).endptr = dst_end;
                        // Write a forwarding pointer after the reverse-pointer.
                        write_forwarding_pointer_at(
                            after_wframe_ptr,
                            dst,
                            dst_end.offset_from(dst) as u16,
                        );
                        forwarded = true;
                        src = after_wframe_ptr;

                        dbgprintln!(
                            "++Hit cauterize at {:?}, remaining worklist: {:?}",
                            src, worklist
                        );
                        break; // no change to src, dst, dst_end
                    }

                    // See Note [Maintaining sharing, Copied and CopiedTo tags].
                    COPIED_TO_TAG | COPIED_TAG => {
                        forwarded = true; // i.e. ALREADY forwarded.
                        let tagged_fwd_ptr = record_time!(
                            {
                                find_forwarding_pointer(
                                    tag,
                                    src,
                                    src_after_tag,
                                )
                            },
                            (*GC_STATS).gc_find_fwdptr_time
                        );
                        let fwd_ptr = tagged_fwd_ptr.untag();
                        let fwd_footer_offset = tagged_fwd_ptr.get_tag();
                        let fwd_footer_addr =
                            fwd_ptr.add(fwd_footer_offset as usize);
                        let space_reqd = 32;
                        let (dst1, dst_end1) = Heap::check_bounds(
                            oldgen, space_reqd, dst, dst_end,
                        );
                        let dst_after_tag = write(dst1, INDIRECTION_TAG);
                        let dst_after_indr =
                            write(dst_after_tag, tagged_fwd_ptr.as_u64());

                        dbgprintln!("   forwarding ptr!: src {:?}, wrote tagged ptr {:?} to dest {:?}",
                                    src, tagged_fwd_ptr, dst);

                        write_shortcut_ptr!(mb_shortcut_addr, dst1, dst_end1);

                        #[cfg(feature = "gcstats")]
                        {
                            (*GC_STATS).mem_copied += 9;
                        }

                        // Update outsets and refcounts if evacuating to the oldest
                        // generation.
                        handle_old_to_old_indirection(
                            dst_end,
                            fwd_footer_addr,
                        );
                        match (*frame).gc_root_prov {
                            GibGcRootProv::RemSet => {}
                            GibGcRootProv::Stk => {
                                let fwd_footer = fwd_footer_addr
                                    as *const GibOldgenChunkFooter;
                                record_time!(
                                    (*(st.zct)).remove(
                                        &((*fwd_footer).reg_info
                                            as *const GibRegionInfo),
                                    ),
                                    (*GC_STATS).gc_zct_mgmt_time
                                );
                                ()
                            }
                        }

                        dst = dst_after_indr;
                        dst_end = dst_end1;
                        let src_after_burned = st.so_env.get(&src);
                        if let Some(next) = worklist.pop() {
                            next_action = next;
                            src = *src_after_burned.unwrap_or_else(|| {
                                panic!(
                                    "Could not find {:?} in so_env {:?}",
                                    src, st.so_env
                                )
                            });
                            continue;
                        } else {
                            // WARNING: allow a corrupt null src return pointer.  Should make it an OPTION.
                            src = *src_after_burned.unwrap_or(&null_mut());
                            dbgprintln!("   forwarding pointer was last, don't need skip-over, src = {:?}", src);
                            break;
                        }
                    }

                    // Indicates end-of-current-chunk in the source buffer i.e.
                    // there's nothing more to copy in the current chunk.
                    // Follow the redirection pointer to the next chunk and
                    // continue copying there.
                    //
                    // POLICY DECISION:
                    // Redirections into oldgen are not inlined in the current version.
                    // See Note [Smart inlining policies].
                    REDIRECTION_TAG => {
                        let (tagged_next_chunk, src_after_next_chunk): (
                            u64,
                            _,
                        ) = read(src_after_tag);
                        let tagged =
                            TaggedPointer::from_u64(tagged_next_chunk);
                        let next_chunk = tagged.untag();

                        dbgprintln!("   redirection ptr!: src {:?}, to next chunk {:?}, inlining_underway={:?}",
                                    src, next_chunk, inlining_underway);

                        if burn {
                            // Add a forwarding pointer in the source buffer.
                            debug_assert!(dst < dst_end);
                            write_forwarding_pointer_at(
                                src,
                                dst,
                                dst_end.offset_from(dst) as u16,
                            );
                            forwarded = true;
                        }

                        // If the next chunk is in the nursery, continue evacuation there.
                        // Otherwise, write a redireciton node at dst (pointing to
                        // the start of the oldgen chunk), link the footers and reconcile
                        // the two RegionInfo objects.
                        //
                        // [2022.09.08]:
                        // TODO: instead of copying objects from oldgen when
                        // inlining is underway, we could write indirections to
                        // point to those values. we would need to:
                        // (1) figure out how many values need to be copied, and
                        // (2) use dummy traversals to find their start addresses.
                        //
                        if st.evac_major
                            || st.nursery.contains_addr(next_chunk)
                            || inlining_underway
                        {
                            dbgprintln!(
                                "   inlining redirected (oldgen) data {:?} -> {:?}",
                                src, next_chunk
                            );

                            if !st.evac_major && inlining_underway {
                                burn = false;
                            }

                            src = next_chunk;

                            #[cfg(feature = "gcstats")]
                            {
                                (*GC_STATS).redirs_inlined += 1;
                            }

                            // Same type, new location to evac from:
                            next_action = EvacAction::ProcessTy(
                                next_ty,
                                mb_shortcut_addr,
                            );
                            continue;
                        } else {
                            cold();

                            // A pretenured object whose next chunk is in the old_gen.
                            // We reconcile the two footers.
                            // TODO: Document how it happens. Also, very buggy.
                            // An alternative is to write an indirection node
                            // which will be way easier.

                            dbgprintln!(
                                "   writing redirection node {:?} -> {:?}",
                                dst,
                                tagged_next_chunk as *const i8
                            );

                            let dst_after_tag = write(dst, REDIRECTION_TAG);
                            let dst_after_redir =
                                write(dst_after_tag, tagged_next_chunk);

                            write_shortcut_ptr!(
                                mb_shortcut_addr,
                                dst,
                                dst_end
                            );

                            #[cfg(feature = "gcstats")]
                            {
                                (*GC_STATS).mem_copied += 9;
                                (*GC_STATS).redirs_not_inlined += 1;
                            }

                            // Link footers.
                            let footer1 = dst_end as *mut GibOldgenChunkFooter;
                            let next_chunk_footer_offset = tagged.get_tag();
                            let footer2 = next_chunk
                                .add(next_chunk_footer_offset as usize)
                                as *mut GibOldgenChunkFooter;

                            dbgprintln!(
                                "   reconciling footers {:?}, {:?} and {:?}, {:?}",
                                (*footer1),
                                *((*footer1).reg_info),
                                (*footer2),
                                *((*footer2).reg_info),
                            );

                            // Reconcile RegionInfo objects.
                            let reg_info1 = (*footer1).reg_info;
                            let reg_info2 = (*footer2).reg_info;
                            (*reg_info2).refcount += (*reg_info1).refcount;
                            (*((*reg_info2).outset))
                                .extend(&*((*reg_info1).outset));
                            (*reg_info2).first_chunk_footer =
                                (*reg_info1).first_chunk_footer;
                            (*footer1).next = footer2;
                            (*footer1).reg_info = reg_info2;

                            dbgprintln!(
                                "   reconciled footer {:?}, {:?}",
                                (*footer1),
                                *((*footer1).reg_info),
                            );

                            // Update ZCT.
                            record_time!(
                                (*(st.zct)).remove(
                                    &(reg_info1 as *const GibRegionInfo)
                                ),
                                (*GC_STATS).gc_zct_mgmt_time
                            );
                            record_time!(
                                (*(st.zct)).insert(reg_info2),
                                (*GC_STATS).gc_zct_mgmt_time
                            );

                            // Stop evacuating.
                            src = src_after_next_chunk as *mut i8;
                            dst = dst_after_redir;
                            // TODO: dst_end??
                            break;
                        }
                    }

                    // Regular datatype, copy.
                    _ => {
                        let packed_info: &&[DataconInfo] =
                            INFO_TABLE.get_unchecked(next_ty as usize);
                        let DataconInfo {
                            scalar_bytes,
                            field_tys,
                            num_shortcut,
                            ..
                        } = packed_info.get_unchecked(tag as usize);

                        dbgprintln!(
                            "   regular datacon, field_tys {:?}",
                            field_tys
                        );

                        let scalar_bytes1 = *scalar_bytes;
                        let num_shortcut1 = *num_shortcut;

                        // Check bound of the destination buffer before copying.
                        // Reserve additional space for a redirection node or a
                        // forwarding pointer.
                        let space_reqd: usize = 32 + scalar_bytes1;
                        {
                            // Scope for mutable variables src_mut and dst_mut,
                            // which are the read and write cursors in the source
                            // and destination buffer respectively.

                            let (mut dst2, dst_end2) = Heap::check_bounds(
                                oldgen, space_reqd, dst, dst_end,
                            );

                            // N.B. it's important to perform this write here
                            // before we advance dst2 past the tag.
                            write_shortcut_ptr!(
                                mb_shortcut_addr,
                                dst2,
                                dst_end2
                            );

                            // Copy the tag. Move cursors past the tag.
                            dst2 = write(dst2, tag);
                            let mut src2 = src_after_tag;

                            // Store the address where shortcut pointers should
                            // be written. Move cursors past shortcut pointers.
                            let src_shortcuts_start = src2;
                            let dst_shortcuts_start = dst2;
                            src2 = src2.add(num_shortcut1 * 8);
                            dst2 = dst2.add(num_shortcut1 * 8);
                            let shortcut_addrs: Vec<Option<*mut i8>> =
                                if num_shortcut1 > 0 {
                                    // If a datatype has shortcut pointers, there
                                    // will be a pointer corresponding to every packed
                                    // field, except the first one. The pointers
                                    // will be laid out immediately after the tag.
                                    // Thus the address of ith shortcut pointer
                                    // is dst_shortcuts_start + (i * 8).
                                    let mut addrs = Vec::new();
                                    addrs.push(None);
                                    // If a shortcut pointer points into oldgen
                                    // we should copy it as it is. Otherwise
                                    // arrange things such that evacuating the
                                    // nursery value updates the shortcut pointer.
                                    //
                                    // TODO: what should happen here if we're
                                    // evacuating oldgen?
                                    for i in 0..num_shortcut1 {
                                        let (tagged_shortcut_dst, _): (
                                            u64,
                                            _,
                                        ) = read(
                                            src_shortcuts_start.add(i * 8),
                                        );
                                        let tagged = TaggedPointer::from_u64(
                                            tagged_shortcut_dst,
                                        );
                                        let shortcut_dst = tagged.untag();

                                        dbgprintln!("   shortcut dst {:?}, in nursery {:?}",
                                                    shortcut_dst, st.nursery.contains_addr(shortcut_dst));

                                        if st
                                            .nursery
                                            .contains_addr(shortcut_dst)
                                        {
                                            dbgprintln!("   nursery shortcut pointer ({:?} -> {:?}) will be updated",
                                                        src_shortcuts_start.add(i * 8), shortcut_dst);

                                            addrs.push(Some(
                                                dst_shortcuts_start.add(i * 8),
                                            ));
                                        } else {
                                            dbgprintln!("   writing an oldgen shortcut pointer {:?} -> {:?}",
                                                        dst_shortcuts_start.add(i * 8), tagged_shortcut_dst as *const i8);

                                            write(
                                                dst_shortcuts_start.add(i * 8),
                                                tagged_shortcut_dst,
                                            );
                                            addrs.push(None);
                                        }
                                    }
                                    addrs
                                } else {
                                    vec![None; field_tys.len()]
                                };
                            debug_assert!(
                                field_tys.len() == shortcut_addrs.len()
                            );
                            dbgprintln!(
                                "   need to write shortcut pointers at: {:?}",
                                shortcut_addrs
                            );

                            // Copy the scalar fields. Move cursors past scalar fields.
                            dst2.copy_from_nonoverlapping(src2, scalar_bytes1);
                            src2 = src2.add(scalar_bytes1);
                            dst2 = dst2.add(scalar_bytes1);

                            #[cfg(feature = "gcstats")]
                            {
                                (*GC_STATS).mem_copied += 1 + scalar_bytes1;
                            }

                            if burn {
                                // Add forwarding pointers:
                                // if there's enough space, write a COPIED_TO tag and
                                // dst's address at src. Otherwise just write a COPIED tag.
                                // After the forwarding pointer, we don't burn the rest of
                                // space previously occupied by scalars because nothing
                                // can point to the scalars directly nor can someone
                                // trying to find a forwarding pointer reach the scalars
                                // as they'll reach this forwarding pointer and stop.
                                if scalar_bytes1 >= 8 || num_shortcut1 > 0 {
                                    dbgprintln!("   forwarding constructor at {:?}, to dst {:?}, scalar bytes {}", src, dst, scalar_bytes1);

                                    debug_assert!(dst < dst_end);

                                    record_time!(
                                        {
                                            write_forwarding_pointer_at(
                                                src,
                                                dst,
                                                dst_end.offset_from(dst)
                                                    as u16,
                                            );
                                        },
                                        (*GC_STATS).gc_burn_time
                                    );

                                    forwarded = true;

                                    #[cfg(feature = "gcstats")]
                                    {
                                        (*GC_STATS).forwarded += 1;
                                    }
                                }
                                // NOTE: Comment this case to disable burned tags:
                                else {
                                    dbgprintln!("   burning non-forwardable data at {:?}, scalar bytes {}", src, scalar_bytes1);

                                    record_time!(
                                        {
                                            let _ = write(src, COPIED_TAG);
                                            // Also burn any scalar bytes that weren't big enough for a pointer:
                                            if scalar_bytes1 >= 1 {
                                                write_bytes(
                                                    src_after_tag,
                                                    COPIED_TAG,
                                                    scalar_bytes1 as usize,
                                                );
                                            }
                                        },
                                        (*GC_STATS).gc_burn_time
                                    );

                                    forwarded = false;

                                    #[cfg(feature = "gcstats")]
                                    {
                                        (*GC_STATS).not_forwarded += 1;
                                        (*GC_STATS).mem_burned +=
                                            1 + scalar_bytes1;
                                    }
                                }
                            }

                            match (*frame).gc_root_prov {
                                GibGcRootProv::RemSet => {
                                    dbgprintln!(
                                        "   pushing SkipoverEnvWrite({:?}) action to stack for ctor, root in remembered set",
                                        src
                                    );
                                    worklist.push(
                                        EvacAction::SkipoverEnvWrite(src),
                                    );
                                }

                                GibGcRootProv::Stk => (),
                            }

                            for (ty, shct) in field_tys
                                .iter()
                                .zip(shortcut_addrs.iter())
                                .rev()
                            {
                                worklist
                                    .push(EvacAction::ProcessTy(*ty, *shct));
                            }

                            dbgprintln!(
                                "   added to worklist, length after this {}, prefix(5): {:?}",
                                worklist.len(),
                                &worklist[..std::cmp::min(5, worklist.len())]
                            );

                            src = src2;
                            dst = dst2;
                            dst_end = dst_end2;

                            worklist_next!(worklist, next_action);
                        }
                    }
                } // End match tag
            }
            EvacAction::SkipoverEnvWrite(pointee) => {
                dbgprintln!(
                    "   performing so_env insert continuation: {:?} to {:?}",
                    pointee,
                    src
                );
                st.so_env.insert(pointee, src);

                worklist_next!(worklist, next_action);
            }
        }
    } // End while

    dbgprintln!(
        "+Finished evacuate_packed: recording in so_env {:?} -> {:?}",
        orig_src,
        src
    );
    // Provide skip-over information for what we just cleared out.
    // FIXME: only insert if it is a non-zero location?
    st.so_env.insert(orig_src, src);

    (src, dst, dst_end, forwarded)
}

fn sort_roots(
    rstack: &GibShadowstack,
    rem_set: &GibRememberedSet,
    // -> Box<dyn Iterator<Item = *mut GibShadowstackFrame>>
) -> Vec<*mut GibShadowstackFrame> {
    // Store all the frames in a vector and sort,
    // see Note [Granularity of the burned addresses table] and
    // Note [Sorting roots on the shadow-stack and remembered set].
    //
    // TODO: sort roots into oldgen and nursery seperately;
    // for oldgen we want to sort using highest depth first,
    // for nursery we want to start with leftmost root first.
    let mut frames_vec: Vec<*mut GibShadowstackFrame> = Vec::new();
    for frame in rstack.into_iter().chain(rem_set.into_iter()) {
        frames_vec.push(frame);
    }
    frames_vec.sort_unstable_by(|a, b| unsafe {
        let tagged_a = TaggedPointer::from_u64((*(*a)).ptr as u64);
        let tagged_b = TaggedPointer::from_u64((*(*b)).ptr as u64);
        let ptr_a: *const i8 = tagged_a.untag();
        let ptr_b: *const i8 = tagged_b.untag();
        (ptr_a).cmp(&ptr_b)
    });
    frames_vec
}

#[inline(always)]
unsafe fn read<A>(cursor: *const i8) -> (A, *const i8) {
    let cursor2 = cursor as *const A;
    (cursor2.read_unaligned(), cursor2.add(1) as *const i8)
}

#[inline(always)]
unsafe fn read_mut<A>(cursor: *mut i8) -> (A, *mut i8) {
    let cursor2 = cursor as *const A;
    (cursor2.read_unaligned(), cursor2.add(1) as *mut i8)
}

#[inline(always)]
unsafe fn write<A>(cursor: *mut i8, val: A) -> *mut i8 {
    let cursor2 = cursor as *mut A;
    cursor2.write_unaligned(val);
    cursor2.add(1) as *mut i8
}

#[macro_export]
macro_rules! worklist_next {
    ($worklist_:ident, $next_action_:ident) => {
        if let Some(next) = $worklist_.pop() {
            $next_action_ = next;
            continue;
        } else {
            break;
        }
    };
}

#[macro_export]
macro_rules! write_shortcut_ptr {
    ($mb_shortcut_addr_:ident, $dst_:ident, $dst_end_:ident) => {
        if let Some(shortcut_addr) = $mb_shortcut_addr_ {
            let offset = $dst_end_.offset_from($dst_);
            let tagged: u64 =
                TaggedPointer::new($dst_, offset as u16).as_u64();
            write(shortcut_addr, tagged);

            dbgprintln!(
                "   wrote tagged shortcut pointer {:?} -> ({:?}, {:?})",
                shortcut_addr,
                $dst_,
                offset
            );
        }
    };
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Managing regions, chunks, metadata etc.
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

impl GibRegionInfo {
    fn new(first_chunk_footer: *const GibOldgenChunkFooter) -> GibRegionInfo {
        GibRegionInfo {
            id: gensym(),
            refcount: 0,
            outset: Box::into_raw(Box::new(HashSet::new())),
            first_chunk_footer: first_chunk_footer,
        }
    }
}

static mut GENSYM_COUNTER: u64 = 0;

/// ASSUMPTION: no parallelism.
#[inline(always)]
fn gensym() -> u64 {
    unsafe {
        let old = GENSYM_COUNTER;
        GENSYM_COUNTER = old + 1;
        old
    }
}

#[inline(always)]
unsafe fn find_forwarding_pointer(
    tag: GibPackedTag,
    addr_of_tag: *const i8,
    addr_after_tag: *mut i8,
) -> TaggedPointer {
    match tag {
        COPIED_TO_TAG => {
            let (tagged, _): (u64, _) = read_mut(addr_after_tag);
            let tagged_fwd_ptr = TaggedPointer::from_u64(tagged);
            dbgprintln!(
                "   Found forwarding ptr at {:?}, dest {:p}",
                addr_after_tag,
                tagged_fwd_ptr.untag() as *const i8
            );
            return tagged_fwd_ptr;
        }
        COPIED_TAG => {
            let (mut scan_tag, mut scan_ptr): (GibPackedTag, *const i8) =
                read(addr_after_tag);
            let offset = 'scan_loop: loop {
                match scan_tag {
                    CAUTERIZED_TAG => {
                        // At this point the scan_ptr is one past the
                        // CAUTERIZED_TAG i.e. at the reverse-pointer
                        // to the shadowstack frame. We must go over
                        // it to reach the COPIED_TO_TAG and then over
                        // that tag to reach the forwarding pointer.
                        scan_ptr = scan_ptr.add(9 as usize);
                        let offset = scan_ptr.offset_from(addr_of_tag)
                            - (1 as isize) // COPIED_TO_TAG
                            - (8 as isize) // reverse pointer
                            - (1 as isize) // CAUTERIZED_TAG
                            ;
                        break 'scan_loop offset;
                    }
                    COPIED_TO_TAG => {
                        // At this point the scan_ptr is one past the
                        // COPIED_TO_TAG i.e. at the forwarding pointer.
                        let offset = scan_ptr.offset_from(addr_of_tag)
                            - (1 as isize) // COPIED_TO_TAG
                            ;
                        break 'scan_loop offset;
                    }
                    COPIED_TAG => {
                        (scan_tag, scan_ptr) = read(scan_ptr);
                    }
                    oth => {
                        panic!("Unexpected tag {:?} found while trying to find forwarding pointer after {:?}",
                               oth,
                               addr_of_tag);
                    }
                }
            };
            // The forwarding pointer that's available.
            let (tagged_fwd_avail, _): (u64, _) = read(scan_ptr);
            let tagged_avail = TaggedPointer::from_u64(tagged_fwd_avail);
            let fwd_avail = tagged_avail.untag();
            let fwd_footer_offset_avail = tagged_avail.get_tag();
            let fwd_footer_addr_avail =
                fwd_avail.add(fwd_footer_offset_avail as usize);

            dbgprintln!(
                "   found forwarding ptr at {:?}, dest {:?}",
                scan_ptr,
                fwd_avail
            );

            // The position in the destination buffer we wanted.
            let fwd_want = fwd_avail.sub(offset as usize);
            // The footers of both forwarding pointers (want and avail) are the same.
            let fwd_footer_offset_want =
                fwd_footer_addr_avail.offset_from(fwd_want);
            return TaggedPointer::new(
                fwd_want,
                fwd_footer_offset_want as u16,
            );
        }
        _ => {
            panic!(
                "find_forwarding_pointer: tag {:?} at {:?} not forwarded",
                tag, addr_of_tag
            );
        }
    }
}

#[inline(always)]
unsafe fn write_forwarding_pointer_at(
    addr: *mut i8,
    fwd: *mut i8,
    tag: u16,
) -> *mut i8 {
    let tagged: u64 = TaggedPointer::new(fwd, tag).as_u64();
    let addr1 = write(addr, COPIED_TO_TAG);
    dbgprintln!(
        "   writing forwarding pointer {:p} at {:?}",
        tagged as *const i8,
        addr1
    );
    write(addr1, tagged)
}

pub unsafe fn free_region(
    footer: *const GibOldgenChunkFooter,
    zct: *mut Zct,
) -> Result<()> {
    #[cfg(feature = "gcstats")]
    {
        // (*GC_STATS).oldgen_regions -= 1;
    }

    // Rust drops this heap allocated object when reg_info goes out of scope.
    let reg_info = Box::from_raw((*footer).reg_info);

    dbgprintln!("Freeing region {:?}, {:?}", (*footer), reg_info);

    // Decrement refcounts of all regions in the outset and add the ones with a
    // zero refcount to the ZCT. Also free the HashSet backing the outset for
    // this region.
    let outset: Box<Outset> = Box::from_raw(reg_info.outset);
    for o_reg_info in outset.into_iter() {
        (*(o_reg_info as *mut GibRegionInfo)).refcount -= 1;
        if (*o_reg_info).refcount == 0 {
            if zct.is_null() {
                free_region((*o_reg_info).first_chunk_footer, zct)?;
            } else {
                (*zct).insert(o_reg_info);
            }
        }
    }

    #[cfg(feature = "verbose_evac")]
    {
        dbgprint!("  chunks: ");
        let mut current_footer = footer;
        let mut start;
        while !current_footer.is_null() {
            start = addr_to_free(current_footer);
            dbgprint!("({:?}, {:?}) ", start, current_footer);
            current_footer = (*current_footer).next;
        }
        dbgprintln!("");
    }

    // Free the chunks in this region.
    let mut free_this = addr_to_free(footer);
    let mut next_chunk_footer = (*footer).next;
    // Free the first chunk and then all others.
    dbgprintln!("  freeing {:?}", free_this);
    libc::free(free_this);
    while !next_chunk_footer.is_null() {
        free_this = addr_to_free(next_chunk_footer);
        next_chunk_footer = (*next_chunk_footer).next;
        dbgprintln!("  freeing {:?}", free_this);
        libc::free(free_this);
    }
    drop(reg_info);
    Ok(())
}

#[inline(always)]
unsafe fn addr_to_free(
    footer: *const GibOldgenChunkFooter,
) -> *mut libc::c_void {
    ((footer as *const i8).sub((*footer).size)) as *mut libc::c_void
}

pub unsafe fn handle_old_to_old_indirection(
    from_footer_ptr: *mut i8,
    to_footer_ptr: *mut i8,
) {
    dbgprintln!(
        "   recording metadata for an old-to-old indirection, {:?}:{:?} -> {:?}:{:?}.",
        from_footer_ptr, *(from_footer_ptr as *const GibOldgenChunkFooter),
        to_footer_ptr, *(to_footer_ptr as *const GibOldgenChunkFooter)
    );

    if from_footer_ptr == to_footer_ptr {
        dbgprintln!(
            "   indirection has identical source and destination, skipping"
        );
        return;
    }

    let added = add_to_outset(from_footer_ptr, to_footer_ptr);
    if added {
        bump_refcount(to_footer_ptr);
    }
}

unsafe fn add_to_outset(from_addr: *mut i8, to_addr: *const i8) -> bool {
    let from_reg_info = (*(from_addr as *mut GibOldgenChunkFooter)).reg_info;
    let to_reg_info = (*(to_addr as *mut GibOldgenChunkFooter)).reg_info;

    dbgprintln!(
        "   adding to outset, {:?}:{:?} -> {:?}:{:?}.",
        from_reg_info,
        *from_reg_info,
        to_reg_info,
        *to_reg_info
    );

    (*((*from_reg_info).outset)).insert(to_reg_info)
}

unsafe fn bump_refcount(addr: *mut i8) -> u16 {
    let reg_info = (*(addr as *mut GibOldgenChunkFooter)).reg_info;
    (*reg_info).refcount += 1;

    dbgprintln!("    bumped refcount, {:?}:{:?}.", reg_info, *reg_info);

    (*reg_info).refcount
}

unsafe fn decrement_refcount(addr: *mut i8) -> u16 {
    let reg_info = (*(addr as *mut GibOldgenChunkFooter)).reg_info;
    (*reg_info).refcount -= 1;

    dbgprintln!("    decremented refcount: {:?}:{:?}", reg_info, *reg_info);

    (*reg_info).refcount
}

pub unsafe fn init_footer_at(
    chunk_end: *mut i8,
    reg_info: *mut GibRegionInfo,
    // Total size of the chunk, *including* space required to store the footer.
    chunk_size: usize,
    refcount: u16,
) -> *mut i8 {
    let footer_space = size_of::<GibOldgenChunkFooter>();
    let footer_start = chunk_end.sub(footer_space);
    let footer: *mut GibOldgenChunkFooter =
        footer_start as *mut GibOldgenChunkFooter;
    let region_info_ptr: *mut GibRegionInfo = if reg_info.is_null() {
        let mut region_info = GibRegionInfo::new(footer);
        region_info.refcount = refcount;
        Box::into_raw(Box::new(region_info))
    } else {
        reg_info
    };
    (*footer).reg_info = region_info_ptr;
    (*footer).size = chunk_size - footer_space;
    (*footer).next = null_mut();

    dbgprintln!(
        "Initialized footer at {:?}: {:?}; {:?}",
        footer_start,
        *footer,
        *region_info_ptr
    );

    footer_start
}

#[inline(always)]
fn is_loc0(addr: *const i8, footer_addr: *const i8, in_nursery: bool) -> bool {
    unsafe {
        let chunk_size: usize = if in_nursery {
            *(footer_addr as *const GibNurseryChunkFooter) as usize
        } else {
            (*(footer_addr as *const GibOldgenChunkFooter)).size
        };
        addr.add(chunk_size) == footer_addr
    }
}

#[inline(always)]
#[cold]
fn cold() {}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Generic memory allocation
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

/// Typeclass for different allocation areas; nurseries, generations etc.
trait Heap {
    fn is_nursery(&self) -> bool;
    fn is_oldest(&self) -> bool;
    fn space_available(&self) -> usize;

    fn allocate(&mut self, size: usize) -> Result<(*mut i8, *mut i8)>;
    fn allocate_first_chunk(
        &mut self,
        size: usize,
        refcount: u16,
    ) -> Result<(*mut i8, *mut i8)>;
    fn allocate_next_chunk(
        &mut self,
        dst: *mut i8,
        dst_end: *mut i8,
    ) -> (*mut i8, *mut i8);

    #[inline(always)]
    fn check_bounds(
        &mut self,
        space_reqd: usize,
        dst: *mut i8,
        dst_end: *mut i8,
    ) -> (*mut i8, *mut i8) {
        debug_assert!(dst < dst_end);
        let space_avail = unsafe { dst_end.offset_from(dst) as usize };
        if space_avail >= space_reqd {
            (dst, dst_end)
        } else {
            cold();
            self.allocate_next_chunk(dst, dst_end)
        }
    }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Nursery
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

impl<'a> GibNursery {
    #[inline(always)]
    fn clear(&mut self) {
        (*self).alloc = (*self).heap_end;
    }

    #[inline(always)]
    fn contains_addr(&self, addr: *const i8) -> bool {
        (addr >= (*self).heap_start) && (addr <= (*self).heap_end)
    }
}

/*

// GC doesn't allocate in the nursery at all.

impl<'a> Heap for GibNursery {
    #[inline(always)]
    fn is_nursery(&self) -> bool {
        true
    }

    #[inline(always)]
    fn is_oldest(&self) -> bool {
        false
    }

    #[inline(always)]
    fn space_available(&self) -> usize {
        unsafe {
            debug_assert!((*self).alloc > (*self).heap_start);
            // alloc - heap_start.
            ptr_offset_from((*self).alloc, (*self).heap_start) as usize
        }
    }

    fn allocate_first_chunk(
        &mut self,
        size: usize,
        _refcount: u16,
    ) -> Result<(*mut i8, *mut i8)> {
        if !self.is_oldest() {
            self.allocate(size)
        } else {
            let total_size = size + size_of::<GibNurseryChunkFooter>();
            let (start, _end) = self.allocate(total_size)?;
            let footer_start = unsafe { start.add(size) };
            let footer = footer_start as *mut GibNurseryChunkFooter;
            unsafe {
                (*footer) = size as GibNurseryChunkFooter;
            }

            #[cfg(feature = "gcstats")]
            {
                unsafe {
                    (*GC_STATS).oldgen_regions += 1;
                }
            }

            Ok((start, footer_start))
        }
    }

    fn allocate_next_chunk(
        &mut self,
        dst: *mut i8,
        dst_end: *mut i8,
    ) -> (*mut i8, *mut i8) {
        unsafe {
            let footer = dst_end as *mut GibNurseryChunkFooter;
            let size: u16 = (*footer);
            let newsize = (size * 2) + size_of::<GibNurseryChunkFooter>();

            let (new_dst, new_dst_end) =
                Heap::allocate(self, newsize).unwrap();

            let footer_start =
                new_dst_end.sub(size_of::<GibNurseryChunkFooter>());
            let footer = footer_start as *mut GibNurseryChunkFooter;
            (*footer) = (size * 2) as GibNurseryChunkFooter;

            // Write a redirection tag in the old chunk.
            let dst_after_tag = write(dst, REDIRECTION_TAG);
            write(dst_after_tag, new_dst);
            (new_dst, new_dst_end)
        }
    }

    #[inline(always)]
    fn allocate(&mut self, size: usize) -> Result<(*mut i8, *mut i8)> {
        let nursery: *mut GibNursery = self;
        unsafe {
            debug_assert!((*nursery).alloc >= (*nursery).heap_start);
            let old = (*nursery).alloc as *const i8;
            let bump: *const i8 = old.sub(size);
            let start = (*nursery).heap_start as *const i8;
            // Check if there's enough space in the nursery to fulfill the
            // request.
            if bump >= start {
                (*nursery).alloc = bump;
                Ok((bump as *mut i8, old as *mut i8))
            } else {
                Err(RtsError::Gc(format!(
                    "nursery alloc: out of space, requested={:?}, \
                     available={:?}",
                    size,
                    old.offset_from(start)
                )))
            }
        }
    }
}

*/

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Old generation
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

impl<'a> GibOldgen<'a> {
    fn collect_regions(&mut self) -> Result<()> {
        let gen: *mut GibOldgen = self;
        unsafe {
            for reg_info in (*((*gen).old_zct)).drain() {
                let footer = (*reg_info).first_chunk_footer;
                // TODO: review ZCT usage.
                if (*reg_info).refcount == 0
                    && !(*((*gen).new_zct)).contains(&reg_info)
                    && !(*((*gen).new_zct)).contains(
                        &((*footer).reg_info as *const GibRegionInfo),
                    )
                {
                    free_region(
                        (*reg_info).first_chunk_footer,
                        (*gen).new_zct,
                    )?;
                }
            }
        }
        self.swap_zcts();
        Ok(())
    }

    fn init_zcts(&mut self) {
        let gen: *mut GibOldgen = self;
        unsafe {
            (*gen).old_zct = &mut *(Box::into_raw(Box::new(HashSet::new())));
            (*gen).new_zct = &mut *(Box::into_raw(Box::new(HashSet::new())));
        }
    }

    fn free_zcts(&mut self) {
        let gen: *mut GibOldgen = self;
        unsafe {
            // Rust will drop these heap objects at the end of this scope.
            let _drop_1 = Box::from_raw((*gen).old_zct);
            let _drop_2 = Box::from_raw((*gen).new_zct);
        }
    }

    fn swap_zcts(&mut self) {
        let gen: *mut GibOldgen = self;
        unsafe {
            let tmp = &mut (*gen).old_zct;
            (*gen).old_zct = (*gen).new_zct;
            (*gen).new_zct = *tmp;
        }
    }
}

impl<'a> Heap for GibOldgen<'a> {
    #[inline(always)]
    fn is_nursery(&self) -> bool {
        false
    }

    #[inline(always)]
    fn is_oldest(&self) -> bool {
        true
    }

    #[inline(always)]
    fn space_available(&self) -> usize {
        0
    }

    fn allocate_first_chunk(
        &mut self,
        size: usize,
        refcount: u16,
    ) -> Result<(*mut i8, *mut i8)> {
        let total_size = size + size_of::<GibOldgenChunkFooter>();
        let (start, end) = self.allocate(total_size)?;

        dbgprintln!("Allocated a oldgen chunk, ({:?}, {:?}).", start, end,);

        let footer_start =
            unsafe { init_footer_at(end, null_mut(), total_size, refcount) };

        #[cfg(feature = "gcstats")]
        {
            unsafe {
                (*GC_STATS).oldgen_regions += 1;
            }
        }

        Ok((start, footer_start))
    }

    fn allocate_next_chunk(
        &mut self,
        dst: *mut i8,
        dst_end: *mut i8,
    ) -> (*mut i8, *mut i8) {
        unsafe {
            // Access the old footer to get the region metadata.
            let old_footer = dst_end as *mut GibOldgenChunkFooter;
            // Allocate space for the new chunk.
            let mut chunk_size =
                ((*old_footer).size + size_of::<GibOldgenChunkFooter>()) * 2;
            if chunk_size > MAX_CHUNK_SIZE {
                chunk_size = MAX_CHUNK_SIZE;
            }
            let (new_dst, new_dst_end) =
                Heap::allocate(self, chunk_size).unwrap();
            // Initialize a footer at the end of the new chunk.
            let reg_info: *mut GibRegionInfo = (*old_footer).reg_info;
            let new_footer_start = init_footer_at(
                new_dst_end,
                reg_info,
                chunk_size,
                (*reg_info).refcount,
            );
            // Write a redirection tag in the old chunk.
            let footer_offset: u16 =
                new_footer_start.offset_from(new_dst) as u16;

            let tagged: u64 =
                TaggedPointer::new(new_dst, footer_offset).as_u64();
            let dst_after_tag = write(dst, REDIRECTION_TAG);
            write(dst_after_tag, tagged);
            // Link the footers.
            let new_footer: *mut GibOldgenChunkFooter =
                new_footer_start as *mut GibOldgenChunkFooter;
            (*old_footer).next = new_footer;
            (new_dst, new_footer_start)
        }
    }

    #[inline(always)]
    fn allocate(&mut self, size: usize) -> Result<(*mut i8, *mut i8)> {
        // let gen: *mut GibOldgen = self.0;
        unsafe {
            let start = libc::malloc(size) as *mut i8;
            if start.is_null() {
                Err(RtsError::Gc(format!("oldest gen alloc: malloc failed")))
            } else {
                let end = start.add(size);

                #[cfg(feature = "gcstats")]
                {
                    (*GC_STATS).mem_allocated += size;
                }

                Ok((start, end))
            }
        }
    }
}

pub fn init_zcts(oldgen: &mut GibOldgen) {
    oldgen.init_zcts();
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Shadow-stack
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

impl<'a> GibShadowstack {
    /// Length of the shadow-stack.
    fn length(&self) -> isize {
        unsafe {
            let (start_ptr, end_ptr) = (
                ((*self).start as *const i8) as *const GibShadowstackFrame,
                ((*self).alloc as *const i8) as *const GibShadowstackFrame,
            );
            debug_assert!(start_ptr <= end_ptr);
            end_ptr.offset_from(start_ptr)
        }
    }

    #[cfg(feature = "verbose_evac")]
    /// Print all frames of the shadow-stack.
    fn print_all(&self, msg: &str) {
        dbgprintln!("{} shadowstack: ", msg);
        for frame in ShadowstackIter::new(self) {
            unsafe {
                dbgprintln!("{:?}", *frame);
            }
        }
    }

    /// Delete all frames from the shadow-stack.
    fn clear(&mut self) {
        (*self).alloc = (*self).start;
    }
}

impl<'a> IntoIterator for &GibShadowstack {
    type Item = *mut GibShadowstackFrame;
    type IntoIter = ShadowstackIter;

    fn into_iter(self) -> Self::IntoIter {
        ShadowstackIter::new(self)
    }
}

/// An iterator to traverse the shadow-stack.
pub struct ShadowstackIter {
    run_ptr: *const i8,
    end_ptr: *const i8,
}

impl ShadowstackIter {
    fn new(cstk: &GibShadowstack) -> ShadowstackIter {
        debug_assert!((*cstk).start <= (*cstk).alloc);
        ShadowstackIter { run_ptr: (*cstk).start, end_ptr: (*cstk).alloc }
    }
}

impl Iterator for ShadowstackIter {
    type Item = *mut GibShadowstackFrame;

    fn next(&mut self) -> Option<Self::Item> {
        if self.run_ptr < self.end_ptr {
            let frame = self.run_ptr as *mut GibShadowstackFrame;
            unsafe {
                self.run_ptr =
                    self.run_ptr.add(size_of::<GibShadowstackFrame>());
            }
            Some(frame)
        } else {
            None
        }
    }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Info table
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

#[derive(Debug, Clone, Default)]
struct DataconInfo {
    /// Bytes before the first packed field.
    scalar_bytes: usize,
    /// Number of shortcut pointer fields.
    num_shortcut: usize,
    /// Number of scalar fields.
    num_scalars: u8,
    /// Number of packed fields.
    num_packed: u8,
    /// Field types of packed fields.
    field_tys: Vec<GibDatatype>,
}

#[derive(Debug, Clone)]
enum DatatypeInfo {
    Scalar(usize),
    Packed(Vec<DataconInfo>),
}

/// The global info table.
static mut _INFO_TABLE: Vec<DatatypeInfo> = Vec::new();

#[inline(always)]
pub fn info_table_initialize(size: usize) {
    unsafe {
        // If a datatype is not packed, info_table_insert_scalar will
        // overwrite this entry.
        _INFO_TABLE = vec![DatatypeInfo::Packed(Vec::new()); size];
    }
}

#[inline(always)]
pub fn info_table_insert_packed_dcon(
    datatype: GibDatatype,
    datacon: GibPackedTag,
    scalar_bytes: usize,
    num_shortcut: usize,
    num_scalars: u8,
    num_packed: u8,
    field_tys: Vec<GibDatatype>,
) -> Result<()> {
    let dcon_info = DataconInfo {
        scalar_bytes,
        num_scalars,
        num_shortcut,
        num_packed,
        field_tys,
    };
    let entry = unsafe { _INFO_TABLE.get_unchecked_mut(datatype as usize) };
    match entry {
        DatatypeInfo::Packed(packed_info) => {
            while packed_info.len() <= datacon.into() {
                packed_info.push(DataconInfo::default());
            }
            packed_info[datacon as usize] = dcon_info;
            Ok(())
        }
        DatatypeInfo::Scalar(_) => Err(RtsError::InfoTable(format!(
            "Expected a packed entry for datatype {:?}, got a scalar.",
            datatype
        ))),
    }
}

pub fn info_table_insert_scalar(datatype: GibDatatype, size: usize) {
    unsafe {
        _INFO_TABLE[datatype as usize] = DatatypeInfo::Scalar(size);
    }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [2022.07.06] does converting a vector to a slice really help performance?

/// The global info table.
static mut INFO_TABLE: &[&[DataconInfo]] = &[];

pub fn info_table_finalize() {
    unsafe {
        let info_table: *mut &[DataconInfo] =
            libc::malloc(_INFO_TABLE.len() * size_of::<&[DataconInfo]>())
                as *mut &[DataconInfo];
        let mut info_table_alloc = info_table;
        for ty in _INFO_TABLE.iter() {
            match ty {
                DatatypeInfo::Scalar(_size) => {
                    // let ty_info = DatatypeInfo::Scalar(*size);
                    // // info_table_alloc.write_unaligned(ty_info);
                    // *info_table_alloc = ty_info;
                    info_table_alloc = info_table_alloc.add(1);
                }
                DatatypeInfo::Packed(_packed_info) => {
                    let packed_info = &_packed_info[..];
                    *info_table_alloc = packed_info;
                    info_table_alloc = info_table_alloc.add(1);
                }
            }
        }
        INFO_TABLE = std::slice::from_raw_parts(info_table, _INFO_TABLE.len());
        dbgprintln!("INFO_TABLE: {:?}", INFO_TABLE);
    }
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Update GC statistics
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

#[cfg(feature = "gcstats")]
#[macro_export]
macro_rules! record_time {
    ( $x:expr, $addr:expr ) => {{
        let start = std::time::Instant::now();
        let y = $x;
        let duration = start.elapsed();
        $addr += duration.as_secs_f64();
        y
    }};
}

#[cfg(not(feature = "gcstats"))]
#[macro_export]
macro_rules! record_time {
    ( $x:expr, $addr:expr ) => {{
        $x
    }};
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Custom error type
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

#[derive(Debug)]
pub enum RtsError {
    InfoTable(String),
    Gc(String),
}

impl Error for RtsError {
    fn description(&self) -> &str {
        match *self {
            RtsError::InfoTable(ref err) => err,
            RtsError::Gc(ref err) => err,
        }
    }
}

impl std::fmt::Display for RtsError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", *self)
    }
}

pub type Result<T> = std::result::Result<T, RtsError>;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Printf debugging functions
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

#[cfg(feature = "verbose_evac")]
/// Print all information about the nursery and oldgen.
pub fn print_nursery_and_oldgen(
    rstack: &GibShadowstack,
    wstack: &GibShadowstack,
    nursery: &GibNursery,
    oldgen: &GibOldgen,
) {
    unsafe {
        let mut info_env: HashMap<*const i8, OldGenerationChunkInfo> =
            HashMap::new();
        let mut add_to_info_env =
            |frame: *const GibShadowstackFrame, is_read: bool| -> () {
                if !nursery.contains_addr((*frame).ptr) {
                    let chunk_end = (*frame).endptr;
                    let footer: *const GibOldgenChunkFooter =
                        chunk_end as *const GibOldgenChunkFooter;
                    let chunk_start = chunk_end.sub((*footer).size);
                    let info = info_env.entry(chunk_end).or_default();
                    (*info).start = chunk_start;
                    (*info).end = chunk_end;
                    if is_read {
                        (*info).read_frames.push(frame);
                    } else {
                        (*info).write_frames.push(frame);
                    }
                }
            };
        for frame in rstack.into_iter() {
            add_to_info_env(frame, true);
        }
        for frame in wstack.into_iter() {
            add_to_info_env(frame, false);
        }
        dbgprintln!("\n--------------------\nNursery:\n--------------------");
        dbgprintln!(
            "start={:?}, end={:?}, alloc={:?}",
            (*nursery).heap_start,
            (*nursery).heap_end,
            (nursery).alloc
        );
        dbgprintln!(
            "--------------------\nNursery chunks:\n--------------------"
        );

        // Print nursery chunks in newest-chunk-first order.
        for (chunk_start, chunk_end, chunk_size) in nursery
            .into_iter()
            .collect::<Vec<(*const i8, *const i8, u16)>>()
            .iter()
            .rev()
        {
            dbgprint!(
                "|{:?}...{:?}|{:?}| -> ",
                chunk_start,
                chunk_end,
                chunk_size
            );
        }
        dbgprintln!("\n");
        dbgprintln!("");
        dbgprintln!("--------------------\nOldgen:\n--------------------");
        dbgprintln!(
            "rem-set={:?}, old-zct={:?}, new-zct={:?}",
            (*oldgen).rem_set,
            (*oldgen).old_zct,
            (*oldgen).new_zct
        );
        (*oldgen).rem_set.print_all("Remembered set");
        dbgprintln!(
            "--------------------\nOldgen chunks:\n--------------------"
        );
        for (_, info) in info_env.iter() {
            dbgprintln!("{}", info);
        }
    }
}

#[cfg(not(feature = "verbose_evac"))]
/// Print all information about the nursery and oldgen.
pub fn print_nursery_and_oldgen(
    _rstack: &GibShadowstack,
    _wstack: &GibShadowstack,
    _nursery: &GibNursery,
    _oldgen: &GibOldgen,
) {
}

#[derive(Debug)]
struct OldGenerationChunkInfo {
    start: *const i8,
    end: *const i8,
    read_frames: Vec<*const GibShadowstackFrame>,
    write_frames: Vec<*const GibShadowstackFrame>,
}

impl Default for OldGenerationChunkInfo {
    fn default() -> Self {
        OldGenerationChunkInfo {
            start: null(),
            end: null(),
            read_frames: vec![],
            write_frames: vec![],
        }
    }
}

impl std::fmt::Display for OldGenerationChunkInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut read_frames_str = Vec::new();
        let mut write_frames_str: Vec<String> = Vec::new();
        let mut add_frame_str = |frame_ref: &*const GibShadowstackFrame,
                                 is_read|
         -> () {
            unsafe {
                let frame = *frame_ref as *const GibShadowstackFrame;
                let footer = (*frame).endptr as *const GibOldgenChunkFooter;
                let reg_info = (*footer).reg_info as *const GibRegionInfo;
                let outset = (*reg_info).outset;
                let formatted = format!(
                    "(frame={:?} ; footer={:?} ; reg_info={:?} ; outset={:?})",
                    *frame, *footer, *reg_info, *outset
                );
                if is_read {
                    read_frames_str.push(formatted);
                } else {
                    write_frames_str.push(formatted);
                }
            }
        };
        for frame_ref in (*self).read_frames.iter() {
            add_frame_str(frame_ref, true);
        }
        for frame_ref in (*self).write_frames.iter() {
            add_frame_str(frame_ref, false);
        }
        write!(f,
               "OldGenerationChunkInfo {{ start: {:?}, end: {:?}, read_frames: {:?}, write_frames: {:?} }}",
               (*self).start, (*self).end, read_frames_str, write_frames_str)
    }
}

impl<'a> IntoIterator for &GibNursery {
    type Item = (*const i8, *const i8, u16);
    type IntoIter = NurseryIter;

    fn into_iter(self) -> Self::IntoIter {
        NurseryIter::new(self)
    }
}

pub struct NurseryIter {
    run_ptr: *const i8,
    end_ptr: *const i8,
}

impl NurseryIter {
    fn new(nursery: &GibNursery) -> NurseryIter {
        NurseryIter { run_ptr: (*nursery).heap_end, end_ptr: (*nursery).alloc }
    }
}

/// Traverses the nursery in oldest-chunk-first order.
impl Iterator for NurseryIter {
    type Item = (*const i8, *const i8, u16);

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if self.run_ptr > self.end_ptr {
                let chunk_end = self.run_ptr;
                let footer_start = self.run_ptr.sub(2);
                let chunk_size: u16 = *(footer_start as *const u16);
                let chunk_start = footer_start.sub(chunk_size as usize);
                debug_assert!(chunk_start < footer_start);
                debug_assert!(footer_start <= chunk_end);
                self.run_ptr = chunk_start;
                Some((chunk_start, chunk_end, chunk_size))
            } else {
                None
            }
        }
    }
}