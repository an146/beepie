open Base

let priorityqueue_of_array arr order =
   let q = PriorityQueue.make order in
   for i = 0 to (Array.length arr) - 1 do
      PriorityQueue.add q i
   done;
   q;;

let enum_merge2 ?(order = compare) e =
   let arr = BatArray.of_enum e in
   let pq_order a b =
      match BatEnum.peek arr.(a), BatEnum.peek arr.(b) with
      | None, _ -> false
      | _, None -> true
      | Some x, Some y -> order x y < 0
   in
   let queue = priorityqueue_of_array arr pq_order in
   let get_element _ =
      let idx = PriorityQueue.first queue in
      let elt = BatEnum.get arr.(idx) in
      PriorityQueue.reorder_down queue idx;
      elt
   in
   BatEnum.from_while get_element;;

(* vim: set ts=3 sw=3 tw=80 : *)
