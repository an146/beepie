open Batteries

val export_events : MidiFile.t -> (int * int * MidiCmd.t) Enum.t
val export_file : MidiFile.t -> string -> unit

(* vim: set ts=3 sw=3 tw=80 : *)
