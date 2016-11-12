

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


