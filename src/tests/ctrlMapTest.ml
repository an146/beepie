open OUnit

let test_empty () =
   let m = CtrlMap.create 13 in
   assert_equal (CtrlMap.get 0 m) 13;;

let test_set () =
   let m = CtrlMap.create 13 in
   let m = CtrlMap.set 100 14 m in
   let m = CtrlMap.set 50 15 m in
   let m = CtrlMap.set 75 16 m in
   assert_equal (CtrlMap.get 49 m) 13;
   assert_equal (CtrlMap.get 50 m) 15;
   assert_equal (CtrlMap.get 74 m) 15;
   assert_equal (CtrlMap.get 75 m) 16;
   assert_equal (CtrlMap.get 99 m) 16;
   assert_equal (CtrlMap.get 100 m) 14;
   assert_equal (CtrlMap.get 101 m) 14;;

let tests =
   "ctrlMap" >::: [
      "empty" >:: test_empty;
      "set" >:: test_set
   ];;

(* vim: set ts=3 sw=3 tw=80 : *)
