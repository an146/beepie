type t = private int * int

val make : int -> int -> t
val create : int -> int -> t
val numer : t -> int
val denom : t -> int
val len : int -> t -> int

(* vim: set ts=3 sw=3 tw=80 : *)
