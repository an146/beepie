open OUnit

module Bst = Bst.Make(struct
	type t = int
	let compare = compare
end)

let rec input_tree n =
   if n <= 0 then
      Bst.empty
   else
      Bst.add (Random.int 10000) (input_tree (n - 1))

let input_tree =
   Random.init 0x12345678;
   input_tree 100000;;

let test_balance () =
   assert_equal 14 (Bst.height input_tree);;

let tests =
	"bst" >::: [
      "balance" >:: test_balance
	];;

(* vim: set ts=3 sw=3 tw=80 : *)
