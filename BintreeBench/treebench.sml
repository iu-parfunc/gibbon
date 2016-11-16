open Timer

         
fun putStrLn (str: string) = print (str ^ "\n")
fun printLargeInt (i: LargeInt.int)    = putStrLn (LargeInt.toString i)
fun printLargeReal (i: LargeReal.real)    = putStrLn (LargeReal.fmt (StringCvt.FIX NONE) i)

datatype tree =
    Leaf of Int64.int
  | Node of tree * tree

type micro     = LargeInt.int
type microreal = LargeReal.real

fun add1Tree (t: tree): tree =
   case t of
      Leaf n        => Leaf (n + 1)
    | Node (t1, t2) => Node (add1Tree t1, add1Tree t2)

fun countLeaves (t: tree) : Int64.int =
   case t of
      Leaf n        => 1
    | Node (t1, t2) => countLeaves t1 + countLeaves t2

fun buildTree (power: int): tree =
   let
      fun graftTree (root: Int64.int, power: int): tree =
         if power = 0
            then Leaf root
            else Node ( graftTree (root, power-1)
                      , graftTree (root + Int64.fromLarge (IntInf.pow (2, power-1)), power-1)
                      )
   in
      graftTree (1, power)
   end

fun buildTree2 (power: int): tree =
   let
      fun graftTree (root: Int64.int, power: int): tree =
         if power = 0
            then Leaf root
            else Node ( graftTree (root, power-1)
                      , graftTree (root, power-1)
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
             ^ Int64.toString n
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

fun benchmarks_build (power: int, trials: int): (microreal * microreal) =
   let
      val _ = print "Benchmarking building"
      val realTimer = startRealTimer()
      fun computeTimes (its: int): tree list =
         if its = 0
            then []
            else 
	      let val _ = print "." in
 	        buildTree2 power :: computeTimes (its-1)
	      end
      val realTimer = startRealTimer()
      val _ = computeTimes trials
      val realTime = checkRealTimer realTimer
      val microseconds = Time.toMicroseconds realTime
      val _ = putStrLn ".Done!"
      val _ = print "BATCHTIME: "
      val _ = printLargeReal ((LargeReal.fromLargeInt microseconds) / 1000000.0)
      val _ = print "\n"
      val meanTime = LargeReal./ ( LargeReal.fromLargeInt microseconds
                                 , LargeReal.fromInt      trials
                                 )
(*      val sorted = FINISHME *)
      val medianTime = 0.0
   in
      (meanTime, medianTime)
   end
       
fun benchmarks (power: int, trials: int): (microreal * microreal) =
   let
      val _ = print "Benchmarking"
      fun computeTimes (its: int, t: tree): tree list =
         if its = 0
            then []
            else
	      let val _ = print "." in
	        add1Tree t :: computeTimes ((its-1), t)
              end
             
      val t = buildTree power
      val realTimer = startRealTimer ()
      val _ = computeTimes (trials, t)
      val realTime = checkRealTimer realTimer
      val microseconds = Time.toMicroseconds realTime
      val _ = putStrLn ".Done!"
      val _ = print "BATCHTIME: "
      val _ = printLargeReal ((LargeReal.fromLargeInt microseconds) / 1000000.0)
      val _ = print "\n"
      val meanTime = LargeReal./ ( LargeReal.fromLargeInt microseconds
                                 , LargeReal.fromInt      trials
                                 )
(*      val sorted = FINISHME *)
      val medianTime = 0.0
   in
      (meanTime, medianTime)
   end

fun run (args : string list): (microreal * microreal) =
  if EQUAL = (String.compare ((hd args), "build"))
     then 
         let
	    val (power,trials) = case map Int.fromString (tl args) of
                    SOME i :: SOME j :: _ => (i,j)
                  | _           => raise Fail "Can't parse number of iterations"
            val _ = print "Benchmark: build tree size 2^"
     	    val _ = putStrLn (Int.toString power)
     	    val _ = print "  trials = "
     	    val _ = putStrLn (Int.toString trials)
     	    val (meanTime,median) = benchmarks_build (power, trials)
     	    val _ = print "Mean time (seconds): "
     	    val _ = printLargeReal (meanTime / 1000000.0)
         in 
	    (meanTime,median)
	 end
     else
	 let
	    val (power,trials) = case map Int.fromString (tl args) of
                    SOME i :: SOME j :: _ => (i,j)
                  | _           => raise Fail "Can't parse number of iterations"
	    val _ = print "Benchmark: add 1 to all leaves of binary tree, size 2^"
     	    val _ = putStrLn (Int.toString power)
     	    val _ = print "  trials = "
     	    val _ = putStrLn (Int.toString trials)
     	    val (meanTime,median) = benchmarks (power, trials)
     	    val _ = print "Mean time (seconds): "
     	    val _ = printLargeReal (meanTime / 1000000.0) 
	 in
	    (meanTime,median)
	 end

val args = CommandLine.arguments ()
val _ = run args