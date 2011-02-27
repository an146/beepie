open Batteries

type 'a t

exception Out_of_range

val create : ?min:'a -> ?max:'a -> 'a -> 'a t
val get : int -> 'a t -> 'a
val set : int -> 'a -> 'a t -> 'a t
val enum : 'a t -> (int * 'a) Enum.t
val bindings : 'a t -> (int * 'a) list
val clear : 'a t -> 'a t
val reset : 'a -> 'a t -> 'a t

(* vim: set ts=3 sw=3 tw=80 : *)
