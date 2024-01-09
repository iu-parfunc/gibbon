module Bench where

import Basics
import Gibbon.Vector 
import Gibbon.PList

type String = Vector Char

data PackedBool = B Int

data DecisionTree = Node PackedBool DecisionTree DecisionTree Inline | Leaf --Inline


mkRandomDecisionTree :: Int -> DecisionTree
mkRandomDecisionTree depth = if depth <= 0 then Leaf
                             else 
                                let randBool = B (mod rand 2) 
                                    leftSubtree = mkRandomDecisionTree (depth-1) 
                                    rightSubtree = mkRandomDecisionTree (depth-1)
			            randString = getRandomString 250
				    inline = Str randString		
                                  in Node randBool leftSubtree rightSubtree inline


mkRandomInline :: Int -> PList Inline
mkRandomInline len = if len <= 0 then Nil
                     else let
                              randString = getRandomString 50
                              inline = Str randString
                              rst = mkRandomInline (len-1)
                            in Cons inline rst

fromInline :: Inline -> String 
fromInline inline = case inline of 
                            Str a -> a

fromBool :: PackedBool -> Int 
fromBool b = case b of 
                B bb -> bb

merge_plist :: PList Inline -> PList Inline -> PList Inline 
merge_plist lst1 lst2 = case lst1 of 
                            Cons x rst -> Cons x (merge_plist rst lst2)
                            Nil -> lst2 


append_plist :: PList Inline -> Inline -> PList Inline 
append_plist lst elem = case lst of 
                              Nil -> (Cons elem) Nil 
                              Cons x rst -> Cons x (append_plist rst elem)

singleton_plist :: Inline -> PList Inline 
singleton_plist elem = (Cons elem) Nil


accumulateDecisions :: DecisionTree -> PList Inline
accumulateDecisions tree = case tree of 
                                Node b left right str -> let bb = fromBool b 
                                                           in if bb == 1
                                                              then 
                                                                let 
 								    vecLeft = accumulateDecisions left
                                                                    vecRight = accumulateDecisions right
								    merged = merge_plist vecLeft vecRight
								  in append_plist merged str
                                                              else 
                                                                let vecLeft = accumulateDecisions left 
                                                                    vecRight = accumulateDecisions right
                                                                  in merge_plist vecLeft vecRight
                                                                       
                                Leaf -> Nil -- (Cons str) Nil


gibbon_main = 
    let tree = mkRandomDecisionTree 18
        vec  = iterate (accumulateDecisions tree) 
     in ()

--- filter 

--- Map 

--- Search 

--- Length 
