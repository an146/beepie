type t

exception Out_of_range

val create : ?min:int -> ?max:int -> int -> t
val get : int -> t -> int
val set : int -> int -> t -> t

(* vim: set ts=3 sw=3 tw=80 : *)
