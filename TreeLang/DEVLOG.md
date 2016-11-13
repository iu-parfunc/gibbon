

[2016.11.12] {Debugging}
----------------------------------------

I believe that one of the simplification passes (e.g. inlinetriv, it's
before unariser), is introducing a bug with projection.


After cursorize:

```Haskell
  ProjE 0
        (LetE ("flatPr58",
               ProdTy [PackedTy "Tree"
                                (),
                       PackedTy "CURSOR_TY"
                                ()],
               LetE ("cursplus1_73",
                     PackedTy "Tree"
                              (),
                     MkPackedE "Leaf"
                               [VarE "add1_tree70"])
                    (LetE ("curstmp74",
                           PackedTy "CURSOR_TY"
                                    (),
                           AppE "WriteInt"
                                (MkProdE [VarE "cursplus1_73",
                                          VarE "flatPk10"]))
                          (MkProdE [VarE "add1_tree70",
                                    VarE "curstmp74"])))
              (VarE "flatPr58"))],
```

Late becomes, after InlineTriv:


```Haskell
 (LetE ("curstmp74",                          -- end_B
        PackedTy "CURSOR_TY" (),
        AppE "WriteInt"
             (MkProdE [VarE "cursplus1_73",
                       VarE "flatPk10"]))
       (LetE ("end_tr1",
              PackedTy "CURSOR_TY"
                       (),
              PrimAppE AddP
                       [ProjE 1
                              (VarE "fnarg71"),  -- A
                        LitE 9])
             (MkProdE [MkProdE [VarE "end_tr1",     -- end_A
                                ProjE 0
                                      (VarE "fnarg71")],  -- B 
                       ProjE 0
                         (VarE "curstmp74")])
```
                       
Here we have an invalid projection on the `curstmp74`, whereas before
it was correctly applied to `flatPr58`.

----------------------------------------

Nope, that was not the problem.  In fact the curstmp74 above was a red
herring, because the cursorize pass was eroneously producing two
duplicated bindings for curstmp74.

So here it goes again.  Starting after inlinePacked:


```Haskell
   (LetE ("flatPk10",
          IntTy,
          PrimAppE AddP
                   [VarE "flatPA54",
                    VarE "n2"])
         (MkProdE [VarE "end_tr1",
                   LetE ("flatPr58",
                         PackedTy "Tree"
                                  (),
                         MkPackedE "Leaf"
                                   [VarE "flatPk10"])
                        (VarE "NAMED_VAL")]))
```

Here's the SECOND place curstmp74 gets bound and the SECOND Leaf
constructor due to code duplication.  (That shoud be InlinePacked's
job, not cursorize!)

```Haskell
    LetE ("flat75",
          ProdTy [],
          MkProdE [])
         (LetE ("flat76",
                ProdTy [PackedTy "CURSOR_TY"
                                 ()],
                ProjE 1
                      (LetE ("flatPr58",
                             ProdTy [PackedTy "Tree"
                                              (),
                                     PackedTy "CURSOR_TY"
                                              ()],
                             LetE ("cursplus1_73",
                                   PackedTy "Tree"
                                            (),
                                   MkPackedE "Leaf"
                                             [VarE "add1_tree70"])
                                  (LetE ("curstmp74",
                                         PackedTy "CURSOR_TY"
                                                  (),
                                         AppE "WriteInt"
                                              (MkProdE [VarE "cursplus1_73",
                                                        VarE "flatPk10"]))
                                        (MkProdE [VarE "add1_tree70",
                                                  VarE "curstmp74"])))
                            (VarE "flatPr58")))
               (ProjE 0
                      (VarE "flat76")))
```

Ok, here there's one bug straight off.  It created a unary "Prod" type
for flat76.  That explains the bogus ProjE 0, because it's trying to
reference a bogus unary tuple.

But where's the code duplication come from?  flat76 is created by
cursorize.  This whole thing is part of a big `(_,_)` dilated value
which has managed to duplicate the Leaf constructor in both the front
and end expressions. ... And that's before any subsequent
flattening/inlining.



[2016.11.13] {Debugging inlineTriv}
----------------------------------------

There's a new flatten, but it really looks like flatten's output
typechecks.  Here it is before inlineTriv:

```Haskel
    (LetE ("unpkcall62",
           ProdTy [PackedTy "CURSOR_TY"
                            (),
                   PackedTy "CURSOR_TY"
                            ()],
           AppE "add1_tree"
                (MkProdE [VarE "flddst58",
                          VarE "y4"]))
          (LetE ("hoistapp39",
                 ProdTy [PackedTy "CURSOR_TY"
                                  (),
                         PackedTy "Tree"
                                  ()],
                 ProjE 1
                       (VarE "unpkcall62"))
```

And then after inlineTriv, we get this:


```Haskell
    (LetE ("unpkcall62",
        ProdTy [PackedTy "CURSOR_TY" (),
                PackedTy "CURSOR_TY" ()],
        AppE "add1_tree"
             (MkProdE [ProjE 1
                             (ProjE 1
                                    (VarE "unpkcall59")),
                       ProjE 0
                             (ProjE 1
                                    (VarE "unpkcall59"))]))
       (MkProdE [MkProdE [ProjE 0
                                (ProjE 1
                                       (VarE "unpkcall62")),
                          ProjE 0
                                (VarE "fnarg50")],
                 ProjE 1
                       (ProjE 1
                               (VarE "unpkcall62"))]))))])
```

Ok, unsurprisingly the problem is still with Cursorize.  It produces
this badly typed 

```Haskell
    LetE ("hoistapp39",
     ProdTy [PackedTy "CURSOR_TY"
                      (),
             PackedTy "Tree"
                      ()],
     LetE ("unpkcall62",
           ProdTy [PackedTy "CURSOR_TY"
                            (),
                   PackedTy "CURSOR_TY"
                            ()],
           AppE "add1_tree"
                (MkProdE [VarE "flddst58",
                          VarE "y4"]))
          (ProjE 1
                 (MkProdE [MkProdE [ProjE 0
                                          (VarE "unpkcall62"),
                                    VarE "flddst58"],
                           ProjE 1
                                 (VarE "unpkcall62")])))
    (VarE "hoistapp39")
```

From the correct input:

```Haskell
    ProjE 1
          (LetE ("hoistapp39",
                 ProdTy [PackedTy "CURSOR_TY"
                                  (),
                         PackedTy "Tree"
                                  ()],
                 AppE "add1_tree"
                      (VarE "y4"))
                (VarE "NAMED_VAL"))
```

Hoistapp39 is supposed to be a tuple.  It's supposed to be the full
function result with end-witneses.  This expression pushes the
`ProjE 1` projcetion into the wrong place:

    (ProjE 1
     (MkProdE [MkProdE [ProjE 0
                              (VarE "unpkcall62"),
                        VarE "flddst58"],
               ProjE 1
                     (VarE "unpkcall62")]))

