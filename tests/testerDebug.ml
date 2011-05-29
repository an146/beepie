open OUnit

let rec run = function
   | TestCase f -> f ()
   | TestList l -> List.iter run l
   | TestLabel (l, t) -> run t

let _ = run Suites.suites

(* vim: set ts=3 sw=3 tw=80 : *)
