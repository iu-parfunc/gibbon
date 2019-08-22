---
layout: default
---

<!-- <div> -->
<!-- <img class="centered img-70" src="static/gibbon.png"> -->
<!-- </div> -->

[Gibbon](https://github.com/iu-parfunc/gibbon/tree/master/gibbon-compiler) 
is an experimental compiler that transforms high-level functional programs
to operate on _serialized data._

Typically, programs that process tree-like data represent trees using pointer-based
data structures in memory (one heap object per-leaf and per-node) because such a 
layout is convenient to manipulate in a high-level programming language. 
This is also generally distinct from the representation of the data in 
serialized form on disk,
which means that a program must perform some sort or marshaling when working with serialized data.
Gibbon _unifies_ the in-memory and serialized formats, transforming recursive
functions to operate _directly_ on serialized data.

Additionally, while the pointer-based structure is efficient
for random access and shape-changing modifications, it can be inefficient 
for traversals that process most or all of a tree in bulk. 
The Gibbon project aims to explore optimizations of recursive tree transforms 
by changing how trees are stored in memory. 

Currently, the Gibbon compiler has multiple front-ends: an s-expression synax
similar to Typed Racket, and a small subset of Haskell.



## Usage

Build the compiler with:

    $ git clone https://github.com/iu-parfunc/gibbon && cd gibbon/gibbon-compiler
    $ stack setup && stack build

Run a sample program from the [examples](https://github.com/iu-parfunc/gibbon/tree/master/gibbon-compiler/examples) directory:

    $ stack exec -- gibbon -r ./demo/Add1.hs


For more options:

    $ stack exec -- gibbon -h


<div id="publications">

<h2>Publications</h2>

<table>
<tr>
<td>ECOOP'17</td>
<td><b> Compiling Tree Transforms to Operate on Packed Representations:<br/></b> Michael Vollmer, Sarah
Spall, Buddhika Chamith, Laith Sakka, Chaitanya Koparkar, Milind Kulkarni, Sam Tobin-Hochstadt, Ryan Newton [<a href="http://drops.dagstuhl.de/opus/volltexte/2017/7273/pdf/LIPIcs-ECOOP-2017-26.pdf" target="_blank">PDF</a>]</td>
</tr>
<tr>
<td>PLDI'19</td>
<td><b> LoCal: A Language for Programs Operating on Serialized Data:<br/></b> Michael Vollmer, Chaitanya Koparkar, Mike Rainey, Laith Sakka, Milind Kulkarni, Ryan R. Newton [<a href="http://recurial.com/pldi19main.pdf" target="_blank">PDF</a>]</td>
</tr>
</table>

</div>
