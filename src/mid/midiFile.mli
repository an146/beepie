open Batteries
open MidiNote

type file

module File : sig
   type t = file

   val create : int -> t
   val division : t -> int
   val add_track : t -> t
   val add_note : ?channel:int -> int -> note -> t -> t
   val channel_owner : int -> t -> int option
   val ctrl_map : (int * Ctrl.t) -> t -> int CtrlMap.t
   val set_ctrl_map : (int * Ctrl.t) -> int CtrlMap.t -> t -> t
   val tempo_map : t -> int CtrlMap.t
   val set_tempo_map : int CtrlMap.t -> t -> t
   val timesig_map : t -> TimeSig.t CtrlMap.t
   val set_timesig_map : TimeSig.t CtrlMap.t -> t -> t
   val enum_notes : ?track:int -> t -> (int * note) Enum.t
   val tracks_count : t -> int
   val tracks : t -> int Enum.t
end

(* vim: set ts=3 sw=3 tw=80 : *)
