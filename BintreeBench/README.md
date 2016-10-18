

[2016.01.06] {Rough initial results}
----------------------------------------

To add1 to the leaves of a binary tree of size 2^20, it took roughly:

 * 200ms in ghc-7.10.2 -O2
 *  90ms in Chez Scheme 8.4
 *  20ms in MLton 20130715
 * 280ms in OCaml 4.02.1

[2016.09.23] {Updating}

Running on my "drone" machine:

 * 110ms - GHC/strict, but highly variable, often 66ms (7.10.3)
 * 85ms - Rust 1.13.0 nightly (no sys alloc)

[2016.09.24]

On first inspection the treebench_unpacked benchmark takes 2ms on my
laptop for 2^20.  This version uses the unpacked (preorder)
representation, and also uses the same input/output memory again and
again.  -RN



