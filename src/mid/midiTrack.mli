open Batteries
open MidiNote

type t

val create : unit -> t
val is_empty : t -> bool
val add_note : note -> int -> t -> t
val owns : int -> t -> bool
val enum : t -> (int * note) Enum.t
val choose_note : t -> (int * note)
val tvalue : Ctrl.t -> t -> int
val set_tvalue : Ctrl.t -> int -> t -> t
val set_volume : int -> t -> t

(* Internal *)
val reset_tvalues : (Ctrl.t, int) PMap.t -> t -> t

(* vim: set ts=3 sw=3 tw=80 : *)
