Big points
----------

 * *Everyone seemed confused about the types.* We should add a citation for
   session types, and clarify why they're useful to us (ie. we want to
   ensure that data is read from the array in exactly the right order). 
   Seems like this confusion might be related to the reviewers thinking
   about indirection pointers?
 * *Everyone seemed confused about handling incomplete or not 
   in-place traversals.* Some comments seem to assume that indirection
   is necessary, for example, or wonder whether subtrees need to be
   traversed multiple times if the pattern match order is backwards.
   We can clear this up. 
 * *Mixed feelings about the evaluation.* I agree that this is a weak point
   of the paper.
 * *Questions about in-place updates to trees.* I guess theoretically we
   could do in-place updates when we don't change the tree structure,
   and maybe even if we do change the structure but also know the exact 
   sizes of everything, but it's kinda beside the point.
 * *Some skepticism about how useful/general the technique is.* Again this
   is a valid criticism and we haven't really demonstrated that it's
   general. We can maybe talk about our hand-written programs like
   the kd-tree benchmark? Certainly this technique can be used for
   things that don't look like map or fold, but we didn't show that.
 * *We cheated in the benchmarks by not using GC.* Maybe "cheat" is
   too strong. Anyway, fixing this should be easy. I already have
   a branch on GitHub of Gibbon with the Boehm GC added to the
   run-time system, so we can re-do the benchmarks with that and
   probably see little difference in performance. 
 
Review 1
--------

Overall this is a very positive review (accept, expert). 

The paper summary seems mostly accurate. One minor point of 
(maybe) confusion is the claim that incomplete traversals
require indirection. We do mention that as an optimization, but
generating a dummy traversal instead is sufficient.

It looks like the main complaint is that we weren't clear enough
about the limitations of Gibbon. 

This reviewer speculates about in-place tree updates, which we didn't
talk about but we could maybe say something about in the response.

We can address the limitations of the evaluation. 

I have no idea what behavioral types are but the reviewer is 
probably right about them.

I doubt we can promise a more formal treatment of the compiler passes,
but we can definitely expand on them more in prose.

Important point to clear up: GC is not included in run time because
there is no GC!

And the approach is automated even without indirections! They are
not needed for full traversals. I don't know where this confusion
came from.

Review 2
--------

This review is quite negative but the criticisms seem valid to me.

The major criticism here is that we don't demonstrate that we
can handle anything more complicated than add1tree. 

Like the previous reviewer, this one wondered about in-place
updates to trees, but doesn't use that language. I read "modify
the structure of packed trees" to mean "modify (in-place)," in
which case the answer is that we probably can't.

We can fix details about the presentation, and we can *maybe*
promise more benchmarks (?), but I doubt we can convince
this reviewer that Gibbon can handle a real compiler pass.

The comment about "not interesting to an OOP audience" is strange
to me. Is there something about the paper itself that limits it
to a FP audience, or is it that we talk about Haskell and Racket?

Review 3
--------

This reviewer isn't an expert.

They were also confused by the type system and didn't understand
the compiler passes.

Promising to clean up the presentation and use more consistent
examples might be good.

Their example code is interesting. Certainly there's no reason
that this function should require redundant traversal, and 
I don't think Gibbon would do that here, though there are 
certainly cases right now where Gibbon will unnecessarily 
duplicate work due to shortcuts taken in the compiler. 
At least theoretically the compiler should treat pattern match
branches as unordered, since there are no effects. 

They also bring up GC, and are correct that the comparison 
is not fair.
