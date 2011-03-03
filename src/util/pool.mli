type 'a t

val create : (unit -> 'a) -> int -> 'a t
val get : 'a t -> 'a
val put : 'a -> 'a t -> unit

(* vim: set ts=3 sw=3 tw=80 : *)
