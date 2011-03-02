open Batteries
open MidiAsm
open OUnit
open Printf
module File = MidiFile

let test_simple_notes () =
   let channel = 0 in
   let file = File.create 240 |> File.add_track in
   let tr = File.track 0 file in
   let file = File.add_note ~channel tr (note 30 (0, 64) (100, 65)) file in
   let file = File.add_note ~channel tr (note 30 (300, 64) (400, 65)) file in
   let pw = File.ctrl_map (0, Ctrl.pitchwheel) file in
   let pw = CtrlMap.set 0   0x2000 pw in (* default, no effect *)
   let pw = CtrlMap.set 50  0x2001 pw in

   (* these are overriden, no effect *)
   let pw = CtrlMap.set 100 0x2002 pw in
   let pw = CtrlMap.set 150 0x2003 pw in
   let pw = CtrlMap.set 200 0x2000 pw in

   let pw = CtrlMap.set 201 0x2004 pw in (* comes into effect at 300 *)
   let file = File.set_ctrl_map (0, Ctrl.pitchwheel) pw file in

   let tempo_map = File.tempo_map file |> CtrlMap.set 300 12345 in
   let file = File.set_tempo_map tempo_map file in

   let events = List.of_enum (Export.export_events file) in
   let printer l =
      let print (time, track, ev) =
         sprintf "%i %i" time track
      in
      List.map print l |> List.interleave "; " |> List.fold_left (^) ""
   in
   assert_equal ~printer [
      0,   0,  on 0 30 64;
      50,  0,  pitchwheel 0 0x2001;
      100, 0,  off 0 30 65;
      300, -1, tempo 12345;
      300, 0,  pitchwheel 0 0x2004;
      300, 0,  on 0 30 64;
      400, 0,  off 0 30 65;
   ] events

let tests =
   "export" >::: [
      "simple-notes" >:: test_simple_notes;
   ];;

(* vim: set ts=3 sw=3 tw=80 : *)
