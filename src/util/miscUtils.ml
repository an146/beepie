open Base

let priorityqueue_of_array arr order =
   let q = PriorityQueue.make order in
   for i = 0 to (Array.length arr) - 1 do
      PriorityQueue.add q i
   done;
   q;;

let enum_merge2 ?(order = compare) e =
   let arr = Array.of_enum e in
   let pq_order a b =
      match Enum.peek arr.(a), Enum.peek arr.(b) with
      | None, _ -> false
      | _, None -> true
      | Some x, Some y -> order x y < 0
   in
   let queue = priorityqueue_of_array arr pq_order in
   let get_element () =
      let idx = PriorityQueue.first queue in
      let oelt = Enum.get arr.(idx) in
      PriorityQueue.reorder_down queue idx;
      match oelt with
      | None -> None
      | Some elt -> Some (idx, elt)
   in
   Enum.from_while get_element;;

(* vim: set ts=3 sw=3 tw=80 : *)
