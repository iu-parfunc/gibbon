
Top level DEVLOG, for testing setup, Racket code, etc
=====================================================

Note, the compiler development log is in `gibbon-compiler/DEVLOG.md`.
Thus, this journal is for matters outside the main compiler dev, such
as testing, CI, etc.

[2017.01.11] {Tweaking testing setup, usual portability problems}
-----------------------------------------------------------------

This morning we've changed various things about the way tests run.
Right now on my laptop I'm seeing this:

    $ ./run_all_tests.sh -j4
    gcc: error: .stack-work/dist/x86_64-linux-nopie/Cabal-1.22.5.0/build/Packed/FirstOrder/Passes/InlinePacked.dyn_o: No such file or directory

I wonder if that's a ghc or stack bug related involving a data race.
It seems to be reproducible for me right now, on commit



[2017.04.24] {New region calculus, add letregion}
-------------------------------------------------

Here is my proposal for a new region calculus.  To render it in Ascii
(lacking overlines), I'll use "*" or ellipses to indicate
repetition.

#### Refined Region+Location calculus

Constraints are stored separately, but typically we will generate
`(Prog,Constraints)`.

    Prog p := DDefs; FDefs; main= e

    DDefs := data D = K T*; ...
    FDefs := f :: S;
             f [l*] (x*) = e

    FunName f := Primitive | Top-level-user-defined-fun 
    Primitive := + | * | ...

    Expr e := x | N | lambda(x:T, ...) . e* | f [l*] e*    
            | return [l*] e
            | if e then e else e | True | False
            | let [l*] x : T = e in e
            | (e,e*) | prj_i e 
            | letregion r in e
            | K @l e* | case e of (K x* -> e); ...

    Locations l = l1 | l2 ... | start(r)
    TypeSchemas S := forall l* . T* -> T
    Types T := Int | Bool | D_l

    Constraint C :=  l ==^r l + A
    ArithExpr A := N | { c | c >= N }
    Lits N := 0 | 1 | 2 ...

Above the "return" form attaches the invisible return location values
wherever we need them.  Booleans and tuples aren't necessary for a
calculus, but bring the above closer to an "IR".

Shorthands:

    l \in r      ==  (l ==^r l)
    l1,l2 \in r  ==  (l1 ==^r l2 + c) \/ (l2 ==^r l1 + c) where c is a free variable c>=0

In the constraint grammar, it would be the refinement {c|c>=0}.

#### Target language:

Next, the target language.  We could keep multi-arg functions here
or get rid of them.  (We will mess up names anyway, as we squish in
location args.)  

    Prog p := FDefs; main= e
    FDefs := f (x:T, ...) : T = e

    Expr e := x | N | Tag N
            | lambda(x:T) . e | f e
            | if e then e else e | True | False
            | let x : T = e in e
            | (e,e*) | prj_i e 
            | letregion r in e | startR(r) | startW(r)
            | caseTag e of (Tag N &x -> e); ...
            | readInt x   | writeInt x e
            | readTag x e | writeTag x e

    Types T := Int | Tag | OutLoc [T*] | InLoc [T*]

    ----- unchanged -----
    ArithExpr A := N | { C | c >= N }
    Lits N := 0 | 1 | 2 ...
    FunName f := Primitive | Top-level-user-defined-fun 
    Primitive := + | * | ...

Here we don't have any different type of linear arrow (or any arrow at
all), but rather we assume that all OutLoc-typed arguments must be
passed linearly.

The above is an adaption of our current L2, and the below of our L3.  
For readability, `Tag N` could just as well be `Tag STR`.


#### Example add1 program:

Here we write add1 which is annotated with locations, but where the
locations are merely metadata, and don't affect the operational
semantics of the program.

This produces output `t2 @ lo_3` which is the same as `t2 @ start(p2)`.

```sml
letregion p1 in
  let [li_1]      t1 = buildTree [startR(p1)] 10 in
  let [li_2,lo_3] t2 = add1 [startR(p1), startW(p2)] t1
  in return [] t2
```

If we run add1 twice, we can free/reuse the first buffer:

```sml
letregion p2 in
  let [li_st4,li_en4] t3 =
                 (letregion p1 in
                   (let [li_1]      t1 = buildTree [startR(p1)] 10 in
                    let [li_2,lo_3] t2 = add1 [startR(p1), startW(p2)] t1
                    in return [startR(p1),li_2] t2))
  in letregion p3 in
     let [li_5,lo_6] t4 = add1 [li_st4, startW(p3)] t1 in
     return [] t4
```

In this program, the storage from `p1` can be reused for `p3`.

