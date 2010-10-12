open IntX
open MidiCmd
open MidiFile
open OUnit

let notes_printer notes =
   string_of_int (List.length (List.hd notes));;

let test_simple_notes () =
   let file =
      Import.import_inline [
         [
            0,   off 0 0 127; (* lonely off *)
            0,   on  0 0 64 ; (* 1 terminated by on *)
            100, on  0 0 127; (* 2 terminated by zero-velocity on *)
            200, on  0 0 0  ;
            200, on  1 2 32   (* 3 terminated in another track *)
         ];
         [
            300, off 1 2 65
         ]
      ]
   in
   let note_end (time, vel) = (time, int7_of_int vel) in
   let note c p on off =
      int4_of_int c, int7_of_int p, note_end on, note_end off
   in
   let notes =
      [
         [
            note 0 0 (0, 64) (100, Import.default_velocity 64);
            note 0 0 (100, 127) (200, Import.default_velocity 127);
            note 1 2 (200, 32) (300, 65)
         ];
         [
         ]
      ]
   in
   let get_track_notes t = NoteSet.elements t.notes in
   let imported_notes = List.map get_track_notes (Array.to_list file#tracks) in
   assert_equal ~printer:notes_printer notes imported_notes;;

let tests =
   "import" >::: [
      "simple-notes" >:: test_simple_notes
   ];;

(* vim: set ts=3 sw=3 tw=80 : *)
