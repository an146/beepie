open Batteries

val read_varlen  : IO.input -> int
val write_varlen : 'a IO.output -> int -> unit

(* vim: set ts=3 sw=3 tw=80 : *)
