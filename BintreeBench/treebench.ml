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

let rec add1_tree = function
  | Leaf i -> Leaf (i + 1)
  | Node (x, y) -> Node (add1_tree x, add1_tree y)

let rec leftmost = function
  | Leaf i -> i
  | Node (x, _) -> leftmost x

let () = begin
  let repeat = 17 in
  let total = ref 0.0 in

  if Array.length Sys.argv <> 2 then
    raise (Failure "Bad command line args. Expected one number (exponent).")
  else begin
    let power = int_of_string (Sys.argv.(1)) in
    for _i = 0 to repeat do
      let tree = build_tree power in

      (* Sys.time is in seconds *)
      let start = Sys.time () in
      let ret = add1_tree tree in
      let end_ = Sys.time() in

      Printf.printf "(Test, leftmost leaf in output: %d)\n" (leftmost ret);
      Printf.printf "Took %f seconds.\n" (end_ -. start);
      total := !total +. (end_ -. start);
    done
  end;

  Printf.printf "Average of %d runs: %f seconds.\n" repeat (!total /. float_of_int repeat);
end
