open OUnit

let suites = "" >::: [
   BstTest.tests
];;

let _ = run_test_tt ~verbose: true suites;;
