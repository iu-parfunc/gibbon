-- List: List (Factored).
-- Functions: mkList, add1, sumList, sumListAcc, length, id.
-- Annotated: MayVectorize on add1; StoreScalarCounts on mkList.
data List = Cons Int64 List | Nil
{-# ANN type List "Factored" #-}

{-# ANN mkList "OPT:StoreScalarCounts" #-}
mkList :: Int64 -> List
mkList length = if length <= 0
		then Nil
		else
		  let rst = mkList (length - 1)
                   in Cons length rst


{-# ANN add1 "OPT:MayVectorize" #-}
add1 :: List -> List
add1 lst = case lst of
		Nil -> Nil
		Cons i rst -> let
				i1 = i + 1
			       in Cons i1 (add1 rst)


sumList :: List -> Int64
sumList lst = case lst of
		   Nil -> 0
		   Cons i rst -> let sumRst = sumList rst
                                  in i + sumRst

sumListAcc :: List -> Int64 -> Int64
sumListAcc lst acc = case lst of
			   Nil -> acc
			   Cons i rst -> sumListAcc rst (acc + i)

length :: List -> Int64
length lst = case lst of
	           Nil -> 0
                   Cons i rst -> 1 + (length rst)

id :: List -> List
id lst = lst

gibbon_main = let
				  _ = printsym (quote "Running program List: ")
				  _ = printsym (quote "NEWLINE")
				  lst = mkList 100000000

				  _ = printsym (quote "Running pass add1 List (map, uses=2, shared=0): ")
				  _ = printsym (quote "NEWLINE")
				  lst' = iterate (add1 lst)
				  _ = printsym (quote "End")
				  _ = printsym (quote "NEWLINE")
				  _ = printsym (quote "Running pass length List (fold, uses=1): ")
                                  _ = printsym (quote "NEWLINE")
                                  len = iterate (length lst)
                                  _ = printsym (quote "End")
                                  _ = printsym (quote "NEWLINE")
				  _ = printsym (quote "Running pass sumList (fold, uses=2): ")
				  _ = printsym (quote "NEWLINE")
				  sum = iterate (sumList lst')
				  _ = printsym (quote "End")
				  _ = printsym (quote "NEWLINE")
				  _ = printsym (quote "Running pass sumList tail recursive (fold, uses=2): ")
				  _ = printsym (quote "NEWLINE")
				  sum' = iterate (sumListAcc lst' 0)
				  _ = printsym (quote "End")
				  _ = printsym (quote "NEWLINE")
				in (sum, sum', len)
