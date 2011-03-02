open Batteries
open MidiAsm
open OUnit
module File = MidiFile

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
   let get_track_notes tr = File.enum_notes ~track:tr file |> List.of_enum in
   let imp_notes = File.tracks file /@ get_track_notes |> List.of_enum in
   assert_equal ~printer:notes_printer notes imp_notes;;

let test_maps () =
   let file =
      Import.import_inline [
         [
            0,   pitchwheel 0 0x1000;
            0,   pitchwheel 0 0x2000;
            0,   ctrl 0 7 10;
            100, tempo 12345;
            100, pitchwheel 0 0x2001;
            150, pitchwheel 0 0x2001;
            200, ctrl 0 7 20;
            200, pitchwheel 0 0x2002;
         ];
         [
         ]
      ]
   in
   let test map values =
      let printer values =
         match values with
         | [] -> ""
         | hd :: tl ->
               let print (t, v) = string_of_int t ^ ", " ^ string_of_int v in
               let concat v1 v2 = v1 ^ "; " ^ (print v2) in
               List.fold_left concat (print hd) tl
      in
      assert_equal ~printer values (CtrlMap.bindings map)
   in
   let c ct = File.ctrl_map (0, ct) file in
   test (c Ctrl.pitchwheel) [100, 0x2001; 200, 0x2002];
   test (c Ctrl.volume) [0, 10; 200, 20];
   test (File.tempo_map file) [100, 12345]

let test_omit_head_track () =
   let file =
      Import.import_inline [
         [
            100, tempo 12345;
         ];
         [
            0,   off 0 30 64;
            100, on  0 30 64;
         ];
      ]
   in
   assert_equal 1 (File.tracks_count file)

let test_volume () =
   let file =
      Import.import_inline [
         [
            100, on  0 30 64;
            200, off 0 30 64;
         ];
         [
            50,  ctrl2 1 Ctrl.volume 123;
            100, on  1 30 64;
            200, off 1 30 64;
         ];
      ]
   in
   assert_equal (File.volume (file, File.track 0 file)) 100;
   assert_equal (File.volume (file, File.track 1 file)) 123

let tests =
   "import" >::: [
      "simple-notes" >:: test_simple_notes;
      "maps" >:: test_maps;
      "omit-head-track" >:: test_omit_head_track;
      "volume" >:: test_volume;
   ];;

(* vim: set ts=3 sw=3 tw=80 : *)
