open Batteries

type file

type note = {
   midipitch : int;
   on_time   : int;
   on_vel    : int;
   off_time  : int;
   off_vel   : int;
}

module File : sig
   type t = file

   val create : int -> t
   val division : t -> int
   val add_track : t -> t
   val add_note : ?channel:int -> int -> note -> t -> t
   val channel_owner : int -> t -> int option
   val ctrlmap : (int * Ctrl.t) -> t -> int CtrlMap.t
   val set_ctrlmap : (int * Ctrl.t) -> int CtrlMap.t -> t -> t
   val enum_notes : ?track:int -> t -> (int * note) Enum.t
   val tracks_count : t -> int
   val tracks : t -> int Enum.t
end

(* vim: set ts=3 sw=3 tw=80 : *)
