open Timer

fun putStrLn (str: string) = print (str ^ "\n")
fun printLargeInt (i: LargeInt.int)    = putStrLn (LargeInt.toString i)
fun printLargeReal (r: LargeReal.real) = putStrLn (LargeReal.toString r)

datatype tree =
    Leaf of int
  | Node of tree * tree

type micro     = LargeInt.int
type microreal = LargeReal.real

fun add1Tree (t: tree): tree =
   case t of
      Leaf n        => Leaf (n + 1)
    | Node (t1, t2) => Node (add1Tree t1, add1Tree t2)

fun buildTree (power: int): tree =
   let
      fun graftTree (root: int, power: int): tree =
         if power = 0
            then Leaf root
            else Node ( graftTree (root, power-1)
                      , graftTree (root + Int.fromLarge (IntInf.pow (2, power-1)), power-1)
                      )
   in
      graftTree (1, power)
   end

fun showTreePrec (p: int, t: tree): string =
   let
      val openParen  = if (p > 10) then "(" else ""
      val closeParen = if (p > 10) then ")" else ""
   in
      case t of
         Leaf n =>
           openParen
             ^ "Leaf "
             ^ Int.toString n
             ^ closeParen
       | Node (t1, t2) =>
           openParen
             ^ "Node "
             ^ showTreePrec (11, t1)
             ^ " "
             ^ showTreePrec (11, t2)
             ^ closeParen
   end
fun showTree (t: tree): string = showTreePrec (0, t)

fun benchmark (power: int): micro =
   let
      val t = buildTree power
      val realTimer = startRealTimer ()
      val _ = add1Tree t
      val realTime = checkRealTimer realTimer
   in
      Time.toMicroseconds realTime
   end

val trials = 17
       
fun benchmarks (power: int): (microreal * microreal) =
   let
      val _ = print "Benchmarking"
      fun computeTimes (its: int): micro list =
         if its = 0
            then []
            else
               let
                  val _ = print "."
               in
                  benchmark power :: computeTimes (its-1)
               end
      val times = computeTimes trials
      val _ = putStrLn ".Done!"
      val timeSum = foldl LargeInt.+ 0 times
      val meanTime = LargeReal./ ( LargeReal.fromLargeInt timeSum
                                 , LargeReal.fromInt      trials
                                 )
(*      val sorted = FINISHME *)
      val medianTime = 0.0
   in
      (meanTime, medianTime)
   end

val power = case map Int.fromString (CommandLine.arguments ()) of
                    SOME i :: _ => i
                  | _           => raise Fail "Can't parse number of iterations"
val _ = print "Benchmark: add 1 to all leaves of binary tree, size 2^"
val _ = putStrLn (Int.toString power)
val _ = print "  trials = "
val _ = putStrLn (Int.toString trials)
val (meanTime,median) = benchmarks power
val _ = print "Mean time (seconds): "
val _ = printLargeReal (meanTime / 1000000.0)
