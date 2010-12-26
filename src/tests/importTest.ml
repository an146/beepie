open MidiAsm
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
            200, on  1 2 32 ; (* 3 terminated in another track *)
            200, on  1 3 100; (* 4 the same on time as 3 *)
            400, off 1 3 100;
         ];
         [
            300, off 1 2 65;
         ]
      ]
   in
   let notes =
      [
         [
            0, note 0 (0, 64) (100, Import.default_velocity 64);
            0, note 0 (100, 127) (200, Import.default_velocity 127);
            1, note 2 (200, 32) (300, 65);
            1, note 3 (200, 100) (400, 100);
         ];
         [
         ]
      ]
   in
   let get_track_notes t = File.enum_notes ~track:t file |> List.of_enum in
   let imp_notes = File.tracks file /@ get_track_notes |> List.of_enum in
   assert_equal ~printer:notes_printer notes imp_notes;;

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
            200, pitchwheel 0 0x2002;
         ];
         [
         ]
      ]
   in
   let test ctrltype values =
      let ctrl = File.ctrlmap (0, ctrltype) file in
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
