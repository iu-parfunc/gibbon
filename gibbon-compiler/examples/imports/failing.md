# Poly
### `/poly/Poly1.hs`

Problem seems to be with the type names, numbers and Nothing, Cons, Nil, Right all match

```
< '#(10 #t 11 #f 2 4 (Poly1_Nothing_77_v_323) (Poly1_Right_76_v_342 20) (Poly1_Right_76_v_334 1) 12 #f 0 3 (Poly1_Cons_74_v_329 1 (Poly1_Cons_74_v_329 2 (Poly1_Nil_73_v_329))) (Poly1_Cons_74_v_329 1 (Poly1_Cons_74_v_329 2 (Poly1_Nil_73_v_329))) (Poly1_Right_76_v_334 1) (Poly1_Cons_74_v_329 11 (Poly1_Cons_74_v_329 12 (Poly1_Nil_73_v_329))))
---
> '#(10 #t 11 #f 2 4 (Nothing_v_295 ) (Right_v_315 20) (Right_v_306 1) 12 #f 0 3 (Cons_v_301 1(Cons_v_301 2(Nil_v_301 ))) (Cons_v_301 1(Cons_v_301 2(Nil_v_301 ))) (Right_v_306 1) (Cons_v_301 11(Cons_v_301 12(Nil_v_301 ))))
\ No newline at end of file
```

# Eval l/r
### `eval_l.hs`

### `eval_r.hs`

these are failing to properly import a module in interp mode this seems to stem from the fact the the imported file does not have a module declaration,,, so internally it renames with "Main" but the true Main module expects "Eval" b/c that's the name in the import statement.

solving this by having the Main module tell each imported module how to name themselves ... this I think could get convoluted b/c a module decalred internally as A could be imported as B ... perhaps a subjective design decision but this seems to work

# ArrowTy
### `layout1ContentSearch.hs`
```
Couldn't match type 'ArrowTy [VectorTy (MetaTv $839)]
                                     IntTy' with 'IntTy'
        Expected type: IntTy
        Actual type: ArrowTy [VectorTy (MetaTv $839)] IntTy
        In the expression: 
          VarE "Gibbon_Vector_length_132" in Var "GenerateLayout1_mkBlogs_layout1_107"
```

This all seems to come from a similair pattern but it looks like the bug doesn't have anything to do with the error. The issue is scope: `Gibbon_Vector_length_132` is some global name but there is a `length` that is one of the function arguements. The rename pass matches length to the global function, causing the error. My solution was to force the global name to be qualified within the scope where `length` exists. So the global `Gibbon.Vector.length` can be accessed if qualified, but simple reference to `length` will use the function arguement `length` 

### `layout1ContentSearchRunPipeline.hs`

### `layout1FilterBlogs.hs`

### `layout1TagSearch.hs`

### `layout2ContentSearch.hs`

### `layout2FilterBlogs.hs`

### `layout2TagSearch.hs`

### `layout3ContentSearch.hs`

### `layout3FilterBlogs.hs`

### `layout3TagSearch.hs`

### `layout4ContentSearch.hs`

### `layout4FilterBlogs.hs`

### `layout4TagSearch.hs`

### `layout5ContentSearch.hs`

### `layout5FilterBlogs.hs`