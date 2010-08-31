open OUnit

let add a b = a + b;;

let text_fixture = "Add" >:::
[
   "add" >:: ( fun () ->
      assert_equal 5 (add 2 3)
   );
]

let _ = run_test_tt ~verbose:true text_fixture

(* vim: set ts=3 sw=3 tw=80 : *)
