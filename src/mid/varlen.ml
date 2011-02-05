open Batteries
open IO

let rec read_varlen input =
   let b = read_byte input in
   let v = b mod 0x80 in
   if b < 0x80 then
      v
   else begin
      let ret = v * 0x80 + (read_varlen input) in
      if ret < 0 then failwith "overflow";
      ret
   end

let write_varlen o n =
   let l = ref [n mod 0x80] in
   let n = ref (n / 0x80) in
   while !n > 0 do
      l := (!n mod 0x80 + 0x80) :: !l;
      n := !n / 0x80
   done;
   List.iter (write_byte o) !l

(* vim: set ts=3 sw=3 tw=80 : *)
