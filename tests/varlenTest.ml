open Batteries
open IO
open OUnit
open Varlen

let test_varlen () =
   let test n s =
      let o = output_string () in
      write_varlen o n;
      let s' = close_out o in
      assert_equal s s';
      assert_equal ~printer:string_of_int n (read_varlen (input_string s))
   in
   test 0 "\x00";
   test 0x7F "\x7F";
   test 0x80 "\x81\x00";
   test 0x123 "\x82\x23";
   test 0x12345 "\x84\xC6\x45"

let tests =
   "varlen" >::: [
      "varlen" >:: test_varlen;
   ]

(* vim: set ts=3 sw=3 tw=80 : *)
