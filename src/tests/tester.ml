open OUnit

let suites = "" >::: [
   BstTest.tests;
   ImportTest.tests
];;

let _ = run_test_tt ~verbose: true suites;;
