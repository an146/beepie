open OUnit

let suites = "" >::: [
   BinaryHeapTest.tests;
   CtrlMapTest.tests;
   ExportTest.tests;
   ImportTest.tests;
   VarlenTest.tests;
];;

(* vim: set ts=3 sw=3 tw=80 : *)
