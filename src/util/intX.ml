let check bits i =
   if i < 0 || i >= (1 lsl bits) then
      invalid_arg ("int" ^ (string_of_int bits) ^ "_of_int")
   else
      i;;

type int4 = Int4 of int
let int4_of_int i = Int4 (check 4 i)
let int_of_int4 = function Int4 i -> i

type int7 = Int7 of int
let int7_of_int i = Int7 (check 7 i)
let int_of_int7 = function Int7 i -> i

type int14 = Int14 of int
let int14_of_int i = Int14 (check 14 i)
let int_of_int14 = function Int14 i -> i

(* vim: set ts=3 sw=3 tw=80 : *)
