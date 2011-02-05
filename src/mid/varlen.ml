open Batteries
open IO

let read_varlen input =
   let rec f acc =
      let b = read_byte input in
      let v = acc * 0x80 + b mod 0x80 in
      if v < 0 then failwith "overflow";
      if b < 0x80 then
         v
      else
         f v
   in
   f 0

let write_varlen o n =
   let l = ref [n mod 0x80] in
   let n = ref (n / 0x80) in
   while !n > 0 do
      l := (!n mod 0x80 + 0x80) :: !l;
      n := !n / 0x80
   done;
   List.iter (write_byte o) !l

(* vim: set ts=3 sw=3 tw=80 : *)
