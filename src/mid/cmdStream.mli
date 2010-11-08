open MidiTypes

type t = (miditime * MidiCmd.t) Stream.t

val parse_stream : char Stream.t -> int -> t

val stream_order : t -> t -> bool

(* vim: set ts=3 sw=3 tw=80 : *)
