type tree =
  | Leaf of int
  | Node of (tree * tree)

let build_tree (n : int) : tree =
  let rec go root n =
    if n == 0 then
      Leaf root
    else
      Node (go root (n - 1), go (root + (2 * (n - 1))) (n - 1))
  in
  go 1 n

let rec sum_tree = function
  | Leaf i -> i
  | Node (x, y) -> (sum_tree x) + (sum_tree y)

let rec add1_tree = function
  | Leaf i -> Leaf (i + 1)
  | Node (x, y) -> Node (add1_tree x, add1_tree y)

let rec leftmost = function
  | Leaf i -> i
  | Node (x, _) -> leftmost x

let () = (


  if Array.length Sys.argv <> 4 then
    raise (Failure "Bad command line args. Expected one string and two numbers (exponent).")
  else (
      let mode = Sys.argv.(1) in
      let power = int_of_string (Sys.argv.(2)) in
      let iters = int_of_string (Sys.argv.(3)) in
      
      if mode = "build" then
        (
          Printf.printf "Benchmark build\n";
          let start = Sys.time () in
          for _i = 0 to iters do
            let tree = build_tree power in
            Printf.printf "(Test, leftmost leaf in output: %d)\n" (leftmost tree);
          done;
          let end_ = Sys.time() in 
              Printf.printf "BATCHTIME: %f\n" (end_ -. start);
        )
      else if mode = "sum" then
        (
          Printf.printf "Benchmark sum\n";
          let start = Sys.time () in
          let tree = build_tree power in
          for _i = 0 to iters do
            let ret = sum_tree tree in
            Printf.printf "Sum: %i\n" ret;
          done;
          let end_ = Sys.time() in
          Printf.printf "Took %f seconds.\n" (end_ -. start);
          Printf.printf "BATCHTIME: %f\n" (end_ -. start);
        )
      else 
        (  
          Printf.printf "Benchmark add1\n";
          let start = Sys.time () in
          let tree = build_tree power in
          for _i = 0 to iters do
            let ret = add1_tree tree in
            Printf.printf "(Test, leftmost leaf in output: %d)\n" (leftmost ret);
          done;
          let end_ = Sys.time() in
          Printf.printf "Took %f seconds.\n" (end_ -. start);
          Printf.printf "BATCHTIME: %f\n" (end_ -. start);
        )
  )
  )
