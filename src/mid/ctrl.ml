type t =
  | Program
  | PitchWheel
  | Controller of int

exception Unsupported

let program = Program
let pitchwheel = PitchWheel

let volume = Controller 7
let balance = Controller 8
let pan = Controller 10
let expression = Controller 11

let data_entry_inc = Controller 96
let data_entry_dec = Controller 97
let nrpn_coarse = Controller 98
let nrpn_fine = Controller 99
let rpn_coarse = Controller 100
let rpn_fine = Controller 101

let all_sound_off = Controller 120
let all_controllers_off = Controller 121
let local_keyboard = Controller 122
let all_notes_off = Controller 123
let omni_mode_off = Controller 124
let omni_mode_on = Controller 125
let monophonic_mode = Controller 126
let polyphonic_mode = Controller 127

let name = function
   | Program -> "program"
   | PitchWheel -> "pitchwheel"
   | Controller n -> "controller" ^ (string_of_int n);;

let default_value ctrltype =
   if ctrltype = pitchwheel then 0x2000
   else if ctrltype = volume then 100
   else if ctrltype = balance || ctrltype = pan then 64
   else 0

let create_map ctrltype =
   let min = 0 in
   let max = if ctrltype = pitchwheel then 0x3FFF else 127 in
   CtrlMap.create ~min ~max (default_value ctrltype);;

let is_supported ctrltype =
   let unsupported = [
      data_entry_inc; data_entry_dec;
      nrpn_coarse; nrpn_fine;
      rpn_coarse; rpn_fine;

      all_sound_off;
      all_controllers_off;
      local_keyboard;
      all_notes_off;
      omni_mode_off; omni_mode_on;
      monophonic_mode; polyphonic_mode;
   ] in
   not (List.mem ctrltype unsupported);;

let check_supported ctrltype =
   if is_supported ctrltype then
      ctrltype
   else
      raise Unsupported

let all_supported =
   let controllers = (0 -- 127) /@ (fun i -> Controller i) // is_supported in
   Program :: PitchWheel :: (List.of_enum controllers)

(* vim: set ts=3 sw=3 tw=80 : *)

