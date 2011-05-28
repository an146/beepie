open Batteries
open MidiFile

type t

val create : unit -> t
val refresh : t -> F.t -> track_id list -> unit
val enum_rows : t -> float -> int Enum.t Enum.t
val measure_width : t -> int -> TabX.t
val row_width : t -> int Enum.t -> TabX.t

(* vim: set ts=3 sw=3 tw=80 : *)
