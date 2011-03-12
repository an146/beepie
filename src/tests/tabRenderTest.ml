open Batteries
open MidiAsm
open MidiFile
open OUnit
open TabRender

let strings = [40; 45; 50; 55; 59; 64]

let test () =
   let f = F.create 240 |> F.add_track in
   let t = F.track 0 f in
   let f = F.add_note t (note ~channel:0 74 (0, 64) (240, 64)) f in
   let f = F.add_note t (note ~channel:0 45 (240, 64) (480, 64)) f in
   Applicature.update (f, t) strings;
   let m = Vect.get (F.measures f) 0 in
   let render = render_measure f [t] m in
   let elt = render |> Enum.skip 1 |> Enum.get |> Option.get in
   assert_equal ~msg:"x_chars" ~printer:string_of_int 2 (fst elt.x);
   assert_equal ~msg:"x_spaces" ~printer:string_of_int 1 (snd elt.x)

let tests =
   "tabRender" >::: [
      "test" >:: test;
   ]

(* vim: set ts=3 sw=3 tw=80 : *)
