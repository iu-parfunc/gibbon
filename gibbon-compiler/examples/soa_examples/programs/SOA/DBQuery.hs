-- DBQuery: Query (Factored).
-- Functions: absI, maxI, wrappingMul, mixSeed, buildQuery, sumCost, sumRows,
-- countJoins. ...
-- Annotated: MayVectorize on scaleCosts, clearQueryFlags; StoreScalarCounts on
-- buildQuery.
data Query
  = Join Int64  -- join type (0=nested-loop,1=hash,2=merge)
         Int64  -- estimated output rows
         Int64  -- total cost
         Int64  -- memory grant
         Query Query
  | Filter Int64  -- predicate id
           Int64  -- selectivity (permille)
           Int64  -- cpu cost
           Int64  -- flags
           Query
  | Scan Int64  -- table id
         Int64  -- base rows
         Int64  -- scan cost
         Int64  -- row width
  | QEmpty

{-# ANN type Query "Factored" #-}

absI :: Int -> Int
absI x = if x < 0 then 0 - x else x

maxI :: Int -> Int -> Int
maxI a b = if a > b then a else b

-- Lightweight arithmetic in plain Gibbon style.
wrappingMul :: Int -> Int -> Int
wrappingMul a b = a * b

mixSeed :: Int -> Int -> Int
mixSeed s salt = s * 1103 + salt * 97 + 13

-- Build a synthetic query plan tree with join/filter/scan operators.
{-# ANN buildQuery "OPT:StoreScalarCounts" #-}
buildQuery :: Int -> Int -> Query
buildQuery d seed =
  if d == 0
  then
    let tableId = mod (absI seed) 17
        rows = 2000 + mod (absI (mixSeed seed 3)) 6000
        cost = 20 + rows / 16
        width = 24 + mod (absI (mixSeed seed 7)) 120
    in Scan tableId rows cost width
  else
    let tag = mod (absI (mixSeed seed 11)) 4
    in if tag < 2
       then
         let l = buildQuery (d - 1) (mixSeed seed 1)
             rDepth = if d > 1 then d - 2 else 0
             r = buildQuery rDepth (mixSeed seed 2)
             joinTy = mod (absI (mixSeed seed 13)) 3
             lRows = 1200 + wrappingMul d 20 + mod (absI (mixSeed seed 17)) 2000
             rRows = 1000 + wrappingMul d 15 + mod (absI (mixSeed seed 19)) 1700
             sel = 60 + mod (absI (mixSeed seed 23)) 260
             outRows = maxI 1 ((wrappingMul lRows rRows) / (sel * 10 + 1))
             joinCpu =
               if joinTy == 0
               then (wrappingMul lRows rRows) / 2400
               else if joinTy == 1
                    then (lRows + rRows) / 7
                    else (lRows + rRows) / 9
             total = 30 + joinCpu + outRows / 20
             mem = if joinTy == 1 then (rRows / 2) else (outRows / 8)
         in Join joinTy outRows total mem l r
       else
         let s = buildQuery (d - 1) (mixSeed seed 3)
             predId = mod (absI (mixSeed seed 29)) 31
             sel = 120 + mod (absI (mixSeed seed 31)) 760
             cpu = 4 + mod (absI (mixSeed seed 37)) 40
             flags = mod (absI (mixSeed seed 41)) 8
         in Filter predId sel cpu flags s

-- Reduction 1: total optimizer cost across the plan tree. Native Int64;
-- also feeds `mapCost1`/`mapCost2` at the bottom of gibbon_main (`sumCost`
-- after `scaleCosts 10`).
sumCost :: Query -> Int
sumCost q =
  case q of
    Join _ _ c _ l r -> c + sumCost l + sumCost r
    Filter _ _ c _ s -> c + sumCost s
    Scan _ _ c _ -> c
    QEmpty -> 0

-- Reduction 2: sum of estimated rows emitted by operators. Native Int64;
-- `childRows * sel` (childRows is a large accumulated sum by depth 75)
-- does not wrap at 32 bits before the division.
sumRows :: Query -> Int
sumRows q =
  case q of
    Join _ r _ _ l s -> r + sumRows l + sumRows s
    Filter _ sel _ _ s ->
      let childRows = sumRows s
          outRows = maxI 1 ((childRows * sel) / 1000)
      in outRows + childRows
    Scan _ r _ _ -> r
    QEmpty -> 0

-- Reduction 3: number of joins in the plan.
countJoins :: Query -> Int
countJoins q =
  case q of
    Join _ _ _ _ l r -> 1 + countJoins l + countJoins r
    Filter _ _ _ _ s -> countJoins s
    Scan _ _ _ _ -> 0
    QEmpty -> 0

-- Reduction 4: memory pressure proxy for execution.
sumMemory :: Query -> Int
sumMemory q =
  case q of
    Join _ _ _ m l r -> m + sumMemory l + sumMemory r
    Filter _ _ c _ s -> c + sumMemory s
    Scan _ _ _ w -> w
    QEmpty -> 0

-- Reduction 5: hash-join spill pressure (uses only join-type + memory).
hashJoinPressure :: Query -> Int
hashJoinPressure q =
  case q of
    Join jt _ _ m l r ->
      let mine = if jt == 1 then m else 0
      in mine + hashJoinPressure l + hashJoinPressure r
    Filter _ _ _ _ s -> hashJoinPressure s
    Scan _ _ _ _ -> 0
    QEmpty -> 0

-- Reduction 6: filter selectivity skew from a 50% baseline.
filterSelectivitySkew :: Query -> Int
filterSelectivitySkew q =
  case q of
    Filter _ sel _ _ s -> absI (sel - 500) + filterSelectivitySkew s
    Join _ _ _ _ l r -> filterSelectivitySkew l + filterSelectivitySkew r
    Scan _ _ _ _ -> 0
    QEmpty -> 0

-- Map 1: scale planner costs for cost-model retuning. `k` is Int64,
-- matching the field it multiplies, so this MayVectorize loop needs no
-- width-changing conversion in its body.
{-# ANN scaleCosts "OPT:MayVectorize" #-}
scaleCosts :: Query -> Int -> Query
scaleCosts q k =
  case q of
    Join t r c m l s ->
      Join t r (c * k) m (scaleCosts l k) (scaleCosts s k)
    Filter p sel c f s ->
      Filter p sel (c * k) f (scaleCosts s k)
    Scan t r c w ->
      Scan t r (c * k) w
    QEmpty ->
      QEmpty

-- Map 2: clear transient filter flags after rewrite.
{-# ANN clearQueryFlags "OPT:MayVectorize" #-}
clearQueryFlags :: Query -> Query
clearQueryFlags q =
  case q of
    Filter p sel c _ sub ->
      Filter p sel c 0 (clearQueryFlags sub)
    Join t r c m l s ->
      Join t r c m (clearQueryFlags l) (clearQueryFlags s)
    Scan t r c w ->
      Scan t r c w
    QEmpty ->
      QEmpty


gibbon_main =
            let _ = printsym (quote "Running program Data base Query: ")
                _ = printsym (quote "NEWLINE")
                queryTree = buildQuery (sizeParam + 75) 17

                _ = printsym (quote "Running pass sumCost (fold, uses=6): ")
                _ = printsym (quote "NEWLINE")
                totCost = iterate (sumCost queryTree)
                _ = printsym (quote "End")
                _ = printsym (quote "NEWLINE")
                _ = printsym (quote "Running pass sumRows (fold, uses=6): ")
                _ = printsym (quote "NEWLINE")
                totRows = iterate (sumRows queryTree)
                _ = printsym (quote "End")
                _ = printsym (quote "NEWLINE")
                _ = printsym (quote "Running pass countJoins (fold, uses=3): ")
                _ = printsym (quote "NEWLINE")
                totJoins = iterate (countJoins queryTree)
                _ = printsym (quote "End")
                _ = printsym (quote "NEWLINE")
                _ = printsym (quote "Running pass sumMemory (fold, uses=6): ")
                _ = printsym (quote "NEWLINE")
                totMem = iterate (sumMemory queryTree)
                _ = printsym (quote "End")
                _ = printsym (quote "NEWLINE")
                _ = printsym (quote "Running pass hashJoinPressure (fold, uses=5): ")
                _ = printsym (quote "NEWLINE")
                hashPressure = iterate (hashJoinPressure queryTree)
                _ = printsym (quote "End")
                _ = printsym (quote "NEWLINE")
                _ = printsym (quote "Running pass filterSelectivitySkew (fold, uses=5): ")
                _ = printsym (quote "NEWLINE")
                selSkew = iterate (filterSelectivitySkew queryTree)
                _ = printsym (quote "End")
                _ = printsym (quote "NEWLINE")
                _ = printsym (quote "Running pass scaleCosts (map, uses=15, shared=9): ")
                _ = printsym (quote "NEWLINE")
                queryTree' = iterate (scaleCosts queryTree 10)
                _ = printsym (quote "End")
                _ = printsym (quote "NEWLINE")
                _ = printsym (quote "Running pass clearQueryFlags (map, uses=14, shared=11): ")
                _ = printsym (quote "NEWLINE")
                queryTree'' = iterate (clearQueryFlags queryTree')
                _  = printsym (quote "End")
                _  = printsym (quote "NEWLINE")
                mapCost1 = sumCost queryTree'
                mapCost2 = sumCost queryTree''
            in (totCost, totRows, totJoins, totMem, hashPressure, selSkew, mapCost1, mapCost2)
