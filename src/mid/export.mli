open MidiFile

val export_events : File.t -> (int * int * MidiCmd.t) Enum.t
val export_file : File.t -> string -> unit

(* vim: set ts=3 sw=3 tw=80 : *)
