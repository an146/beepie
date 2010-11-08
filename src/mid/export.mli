open MidiFile
open MidiTypes

val export_stream : file -> (miditime * int * MidiCmd.t) Stream.t

val export_file : file -> string -> unit

(* vim: set ts=3 sw=3 tw=80 : *)
