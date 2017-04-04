We thank the reviewers for their time and detailed feedback. We will
certainly put significant effort into improving the presentation of
the technical aspects of the paper, following the suggestions of all
three reviewers. In addition to adding more thorough running
examples, as suggested by reviewer C, we will provide a more formal
presentation of the transformations performed by Gibbon.

In this work, we focus on out-of-place transforms for traversals that
touch the vast majority of the tree. As Reviewer A points out,
"mostly-packed" trees and tree-updates are indeed a good topic for
future work.


### Reviwer A 

> performance of other common tree operations like find. 

Our point-correlation is one example of a random-access traversal,
representative of find operations on search trees.

In general, yes, Gibbon's representation transformation is indeed a
high-stakes optimization---it can make performance much worse if
applied inappropriately. We believe this work raises many questions
for further inquiry, including how to derive an effective static or
dynamic policy for *when* to engage the alternate representation.

No program in the input language is untransformable, but a bad choice of
dummy-traversal/copying versus layout/indirections can result in poor
performance. We will add examples to the evaluation that illustrate poor
combinations. For instance, computing a "rightmost leaf" without turning on
layout information creates an asymptotic slowdown. We can also create AST
traversals that avoid walking a given percentage of the tree. If these unchanged
portions are *copied* (rather than an indirection node to existing data), this
creates extra work, and we can identify the point at which that copying cost
outweighs the benefits.

> session types

We will add citations and clarify that these types are only to help
ensure the correctness of the compiler. They enforce type safe use of
memory buffers---reading and writing the expected fields in the right
order and offsets.

> Figure 7 ... exhausted the last-level cache also on packed data?

We will include more details to show how, yes, the transition occurs when the
tree exceeds cache.  At 10^8, 100MB of packed data, all implementations are
reading from RAM, but we can go out to 10^9 too.  Including peak memory usage is
a good idea and we will do that.

> Is GC time included in run-times? 

While Gibbon's "C pointer" mode uses malloc/free, both packed and bumpalloc
modes are using completely arena-based memory management (similar to Ur/Web).
For example, temporary packed data that doesn't escape a lexical scope is freed
at the end of that lexical scope.  Incorporating the Gibbon optimization in a
GC'd language should offer additional advantages in avoiding GC traversals of
packed data (as in our ICFP'15 paper on Compact Normal Form).

> block-chains / packed-indirections

We will clarify our terminology: recording the size of subtrees only works
intra-buffer, but embedding full pointers in the packed representations will
reach between buffers.  Because the compiler knows when it is missing "end
witnesses", that drives including layout information (i.e. as on optimization,
instead of the naive dummy-traversal insertion of section 4.2, which can be
complete/automated even without layout).

The current performance is representative of large blocks capped with guard
pages.  Future work to support finer-grained blocks will require bounds checks
and will have some overhead.  For fairness, bounds-checking overhead is absent
from both the packed and bumpalloc variants currently.


### Reviewer B

> not much of an improvement if the trees fit in the cache,

While the other speedups in the paper are much more dramatic, we believe ~50%
speedups on smaller trees are still large by the standards of compiler
optimizations.

> few realistic problems fit into this form

Yes we focus on out-of-place traversals, but we find that these are common.
Firstly, in functional compilers, but also anywhere we traverse a tree to
produce a tree of a *different type* (i.e. between two IRs).

We have implemented operations such as copy-propogation and substitution (over
the Racket AST) in Gibbon, which follow the map-like pattern, as well as a
type-checker.  In compiler passes, and in search trees, we find examples where
there is no heavy work to amortize the cost of the traversals themselves.

> OOP audience

While the CFP lists a broad range of programming topics, we would also hope to
appeal to the OOP audience with the radical idea of automatically aggregating
subgraphs of heap objects into a single object with a specialized memory
representation.  The small language we present is a subset of many existing
languages that we believe are of interest to the audience (Scala, F#, Swift,
Rust, etc).


### Reviewer C

Again, we will strive to clarify the presentation and we can formally describe
the key transformations in the core language, using extra pages or as an
appendix.

> many different traversal strategies applied to the same ADT

Yes!  As we scale up the approach, we expect to use a semi-automated method to
decide where to convert between different representations of the same data
within a large program.

> Node x y -> sum y + sum x

This program is compiled the same irrespective of the textual order in the code.
Once the effect analysis identifies that a traversal occurs, we aggressively
reorganize and reorder the program to connect end-witnesses to their consumers.
This is made easier by the current language being pure.

> GC/deallocation

See above re: arena deallocation / region-discipline.

