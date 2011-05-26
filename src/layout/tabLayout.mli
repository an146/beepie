open Batteries
open MidiFile
open MidiNote

type elt_value = [
   | `Nothing
   | `Note of note
]

type tabx = float * float (* chars * spaces *)

val (+:) : tabx -> tabx -> tabx
val tabx_max : tabx -> tabx -> tabx

type elt = {
   x : tabx;
   y : int;
   track : track_id;
   text : string;
   value : elt_value;
}

val render_measure : F.t -> track_id list -> measure -> elt Enum.t

(* vim: set ts=3 sw=3 tw=80 : *)
