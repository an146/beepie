open MidiCmd
open OUnit

let notes_printer notes =
   string_of_int (List.length (List.hd notes));;

let test_simple_notes () =
   let file =
      Import.import_inline [
         [
            0,   NoteOff (0, 0, 127); (* lonely off *)
            0,   NoteOn  (0, 0, 64);  (* 1 terminated by on *)
            100, NoteOn  (0, 0, 127); (* 2 terminated by zero-velocity on *)
            200, NoteOn  (0, 0, 0);
            200, NoteOn  (0, 0, 32)   (* 3 terminated in another track *)
         ];
         [
            300, NoteOff (0, 0, 65)
         ]
      ]
   in
   let notes =
      [
         [
            0, 0, (0, 64), (100, Import.default_velocity 64);
            0, 0, (100, 127), (200, Import.default_velocity 127);
            0, 0, (200, 32), (300, 65)
         ];
         [
         ]
      ]
   in
   let get_track_notes t = MidiFile.NoteSet.elements t#notes in
   let imported_notes = List.map get_track_notes file#tracks in
   assert_equal ~printer:notes_printer notes imported_notes;;

let tests =
   "import" >::: [
      "simple-notes" >:: test_simple_notes
   ];;

(* vim: set ts=3 sw=3 tw=80 : *)
