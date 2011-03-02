open Batteries
open MidiNote

type track_id = private int

type measure = {
   start : int;
   len : int;
   notes : (int * note) list;
}

type file

module F : sig
   type t = file

   val create : int -> t
   val division : t -> int

   val track : int -> t -> track_id
   val tracks : t -> track_id Enum.t
   val tracks_count : t -> int
   val track_index : (t * track_id) -> int
   val add_track : t -> t
   val remove_track : track_id -> t -> t
   val channels : track_id -> t -> int Enum.t
   val channel_owner : int -> t -> track_id option

   val measures : t -> measure Vect.t
   val add_note : ?channel:int -> track_id -> note -> t -> t
   val enum_notes : ?track:track_id -> t -> (int * note) Enum.t

   val ctrl_map : (int * Ctrl.t) -> t -> int CtrlMap.t
   val set_ctrl_map : (int * Ctrl.t) -> int CtrlMap.t -> t -> t
   val tempo_map : t -> int CtrlMap.t
   val set_tempo_map : int CtrlMap.t -> t -> t
   val timesig_map : t -> TimeSig.t CtrlMap.t
   val set_timesig_map : TimeSig.t CtrlMap.t -> t -> t
   val tvalue : Ctrl.t -> (t * track_id) -> int
   val set_tvalue : Ctrl.t -> int -> (t * track_id) -> t
   val volume : (t * track_id) -> int
   val set_volume : int -> (t * track_id) -> t
end

(* vim: set ts=3 sw=3 tw=80 : *)
