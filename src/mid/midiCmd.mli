open Batteries

type t = MidiCmdT.t

(*val channel : voice_t -> int*)
val read : ?running_status : int ref -> IO.input -> t
val write : ?running_status : int ref -> 'a IO.output -> t -> unit

(* vim: set ts=3 sw=3 tw=80 : *)
