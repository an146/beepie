open Batteries

val default_velocity : int -> int

val import_inline : ?division:int ->
   (int * MidiCmd.t) list list -> MidiFile.t

val import_events : ?division:int ->
   (int * int * MidiCmd.t) Enum.t -> MidiFile.t

val import_file : string -> MidiFile.t

(* vim: set ts=3 sw=3 tw=80 : *)
