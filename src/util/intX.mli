type int4 = private Int4 of int
val int4_of_int : int -> int4
val int_of_int4 : int4 -> int

type int7 = private Int7 of int
val int7_of_int : int -> int7
val int_of_int7 : int7 -> int

type int14 = private Int14 of int
val int14_of_int : int -> int14
val int_of_int14 : int14 -> int

(* vim: set ts=3 sw=3 tw=80 : *)
