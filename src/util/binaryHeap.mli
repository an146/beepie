type 'a t

val make : ('a -> 'a -> int) -> int -> 'a t
val length : 'a t -> int
val is_empty : 'a t -> bool

val top : 'a t -> 'a
val push : 'a t -> 'a -> unit
val drop : 'a t -> unit
val pop : 'a t -> 'a
val reorder_top : 'a t -> unit
