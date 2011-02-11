open Batteries
open MidiNote

type t

val create : unit -> t
val is_empty : t -> bool
val add_note : note -> int -> t -> t
val owns : int -> t -> bool
val enum : t -> (int * note) Enum.t

(* vim: set ts=3 sw=3 tw=80 : *)
