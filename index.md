---
layout: default
---

<!-- <div> -->
<!-- <img class="centered img-70" src="static/gibbon.png"> -->
<!-- </div> -->

[Gibbon](https://github.com/iu-parfunc/gibbon/tree/master/gibbon-compiler) is a full program compiler that optimizes traversals over algebraic data types by compiling them to operate directly on a pointer-free serialized representation of the data. Programs using such "packed" representations run significantly faster than the ones using pointers. Since the ECOOP'17 publication, we've developed a "location calculus", which formalizes the memory model, and would allow us to prove some interesting properties about it. We're now taking the first steps toward adding support for efficient parallel computations.

## Usage

Build the compiler with:

    $ git clone https://github.com/iu-parfunc/gibbon && cd gibbon/gibbon-compiler
    $ stack setup && stack build

Run a sample program from the [examples](https://github.com/iu-parfunc/gibbon/tree/master/gibbon-compiler/examples) directory:

    $ stack exec -- gibbon -r examples/test02d_printPair.gib


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
