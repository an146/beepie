open OUnit

let suites = "" >::: [
   CtrlMapTest.tests;
   ImportTest.tests
];;

let _ = run_test_tt ~verbose: true suites;;

(* vim: set ts=3 sw=3 tw=80 : *)
