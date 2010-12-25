open OUnit

let test_heap_sort () =
   let random_ints = Random.State.enum_int (Random.State.make [|0|]) 100 in
   let data = random_ints |> Enum.take 1000 |> Array.of_enum in
   let heap = BinaryHeap.make compare 1000 in
   Array.iter (BinaryHeap.push heap) data;
   Array.sort compare data;
   let get_top () =
      if BinaryHeap.is_empty heap then raise Enum.No_more_elements;
      BinaryHeap.pop heap
   in
   let c = Enum.compare compare (Array.enum data) (Enum.from get_top) in
   assert_equal c 0

let tests =
   "binaryHeap" >::: [
      "heapsort" >:: test_heap_sort;
   ]

(* vim: set ts=3 sw=3 tw=80 : *)
