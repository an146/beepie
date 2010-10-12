open OUnit

let suites = "" >::: [
   ImportTest.tests
];;

let _ = run_test_tt ~verbose: true suites;;
