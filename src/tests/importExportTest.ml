open Batteries
open MidiAsm
open OUnit

let test_invariants () =
   let test cmds =
      let file = Import.import_events (List.enum cmds) in
      let cmds' = Export.export_events file in
      assert_equal (Enum.compare compare (List.enum cmds) cmds') 0
   in
   test [
      0,   -1, timesig 3 4;
      0,   0,  on 0 30 64;
      50,  0,  pitchwheel 0 0x2001;
      100, 0,  off 0 30 65;
      300, -1, tempo 12345;
      300, 0,  pitchwheel 0 0x2004;
      300, 0,  on 0 30 64;
      400, 0,  off 0 30 65;
   ]

let tests =
   "importExport" >::: [
      "invariants" >:: test_invariants;
   ]

(* vim: set ts=3 sw=3 tw=80 : *)
