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

let test_ctrls () =
   let file =
      Import.import_inline [
         [
            0,   pitchwheel 0 0x1000;
            0,   pitchwheel 0 0x2000;
            0,   ctrl 0 7 10;
            100, pitchwheel 0 0x2001;
            150, pitchwheel 0 0x2001;
            200, ctrl 0 7 20;
            200, pitchwheel 0 0x2002
         ];
         [
         ]
      ]
   in
   let test ctrltype values =
      let channel = file#channel 0 in
      let ctrl = channel#ctrl ctrltype in
      let printer values =
         match values with
         | [] -> ""
         | hd :: tl ->
               let print (t, v) = string_of_int t ^ ", " ^ string_of_int v in
               let concat v1 v2 = v1 ^ "; " ^ (print v2) in
               List.fold_left concat (print hd) tl
      in
      assert_equal ~printer (CtrlMap.bindings ctrl) values
   in
   test Ctrl.pitchwheel [100, 0x2001; 200, 0x2002];
   test Ctrl.volume [0, 10; 200, 20];;

let tests =
   "import" >::: [
      "simple-notes" >:: test_simple_notes;
      "ctrls" >:: test_ctrls
   ];;

(* vim: set ts=3 sw=3 tw=80 : *)
