open Batteries
open MidiNote

type t

val create : int -> t
val division : t -> int
val add_track : t -> t
val remove_track : int -> t -> t
val map_track : int -> (MidiTrack.t -> MidiTrack.t) -> t -> t
val add_note : ?channel:int -> int -> note -> t -> t
val channel_owner : int -> t -> int option
val ctrl_map : (int * Ctrl.t) -> t -> int CtrlMap.t
val set_ctrl_map : (int * Ctrl.t) -> int CtrlMap.t -> t -> t
val tempo_map : t -> int CtrlMap.t
val set_tempo_map : int CtrlMap.t -> t -> t
val timesig_map : t -> TimeSig.t CtrlMap.t
val set_timesig_map : TimeSig.t CtrlMap.t -> t -> t
(* val enum_notes : ?track:int -> t -> (int * note) Enum.t *)
val tracks_count : t -> int
val tracks : t -> MidiTrack.t Enum.t
val track : int -> t -> MidiTrack.t
val get_note_ctrl : (int * note) -> Ctrl.t -> t -> int

(* Internal *)
val reset_tvalues : t -> t

(* vim: set ts=3 sw=3 tw=80 : *)
