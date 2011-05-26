open OUnit

let suites = "" >::: [
   BinaryHeapTest.tests;
   BinarySearchTest.tests;
   CtrlMapTest.tests;
   ExportTest.tests;
   ImportTest.tests;
   ImportExportTest.tests;
   TabLayoutTest.tests;
   VarlenTest.tests;
];;

(* vim: set ts=3 sw=3 tw=80 : *)
