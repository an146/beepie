open Batteries
open MidiFile
open MidiNote

type elt_value = [
   | `Nothing
   | `Note of note
]

type elt = {
   x : TabX.t;
   y : int;
   track : track_id;
   text : string;
   value : elt_value;
}

val render_measure : F.t -> track_id list -> measure -> elt Enum.t

(* vim: set ts=3 sw=3 tw=80 : *)
