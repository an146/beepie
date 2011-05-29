open Batteries
open OUnit

let test_binary_search () =
   let test i n =
      let i' = MiscUtils.binary_search (fun x -> x >= i) 0 n in
      assert_equal i i'
   in
   for n = 1 to 5 do
      for i = 0 to n - 1 do
         test i n
      done
   done

let tests =
   "binarySearch" >::: [
      "binary-search" >:: test_binary_search;
   ]

(* vim: set ts=3 sw=3 tw=80 : *)
