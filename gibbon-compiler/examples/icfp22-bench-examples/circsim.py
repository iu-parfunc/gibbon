cs = ["None","Inp","Outp","Dff","Inv","And2","Or2","Xor"]
for x in cs:
    print(f"compare_{x} :: Component -> Bool")
    print(f"compare_{x} b = case b of")
    for y in cs:
        print(f"        {y} ->", "True" if x == y else "False")
print(f"eqC :: Component -> Component -> Bool")
print("eqC a b = case a of ")
for x in cs:
    print(f"    {x} -> compare_{x} b")