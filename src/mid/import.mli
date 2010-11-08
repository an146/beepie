open MidiFile
open MidiTypes

val default_velocity : int -> int

val import_inline : ?division:int -> (miditime * MidiCmd.t) list list -> file

val import : string -> file

(* vim: set ts=3 sw=3 tw=80 : *)
