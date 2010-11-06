type t =
  | Program
  | PitchWheel
  | Controller of int

let program = Program
let pitchwheel = PitchWheel

let volume = Controller 7
let balance = Controller 8
let pan = Controller 10
let expression = Controller 11

let create_map ctrltype =
   let min = 0 in
   let max = if ctrltype = pitchwheel then 0x3FFF else 127 in
   let default =
      if ctrltype = pitchwheel then 0x2000
      else if ctrltype = volume then 100
      else if ctrltype = balance || ctrltype = pan then 64
      else 0
   in
   CtrlMap.create ~min ~max default;;

(* vim: set ts=3 sw=3 tw=80 : *)

