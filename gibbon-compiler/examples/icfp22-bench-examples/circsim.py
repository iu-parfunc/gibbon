cs = ["None","Inp","Outp","Dff","Inv","And2","Or2","Xor"]
print("case a of ")
for x in cs:
    print(f"    {x} ->")
    print(f"        case b of")
    for y in cs:
        print(f"            {y} ->", "True" if x == y else "False")

""" 
case a of 
  v -> 

 """