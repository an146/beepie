open Batteries
open MidiFile

val export_events : file -> (int * int * MidiCmd.t) Enum.t
val export_file : file -> string -> unit

(* vim: set ts=3 sw=3 tw=80 : *)
