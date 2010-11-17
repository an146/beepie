type t

exception Out_of_range

val create : ?min:int -> ?max:int -> int -> t
val get : MidiTypes.miditime -> t -> int
val set : MidiTypes.miditime -> int -> t -> t
val enum : t -> (MidiTypes.miditime * int) BatEnum.t
val bindings : t -> (MidiTypes.miditime * int) list

(* vim: set ts=3 sw=3 tw=80 : *)
