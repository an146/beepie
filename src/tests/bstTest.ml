open OUnit

let test_add_ints () =
   assert_equal (2 + 3) (Bst.add_ints 2 3);;

let tests =
	"bst" >::: [
      "add-ints" >:: test_add_ints
	];;

(* vim: set ts=3 sw=3 tw=80 : *)
