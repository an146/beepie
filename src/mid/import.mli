open MidiFile

val default_velocity : int -> int

val import_inline : ?division:int -> (int * MidiCmd.t) list list -> file

val import_file : string -> file

(* vim: set ts=3 sw=3 tw=80 : *)
