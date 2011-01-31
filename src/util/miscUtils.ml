open Base
open Batteries

let priorityqueue_of_array arr order =
   let q = PriorityQueue.make order in
   for i = 0 to (Array.length arr) - 1 do
      PriorityQueue.add q i
   done;
   q;;

let enum_merge2 cmp e =
   let arr = Array.of_enum e in
   let pq_order a b =
      match Enum.peek arr.(a), Enum.peek arr.(b) with
      | None, _ -> false
      | _, None -> true
      | Some x, Some y -> cmp x y < 0
   in
   let queue = priorityqueue_of_array arr pq_order in
   let get_element () =
      let idx = PriorityQueue.first queue in
      let elt = Enum.get arr.(idx) in
      PriorityQueue.reorder_down queue idx;
      elt
   in
   Enum.from_while get_element;;

let enum_add_prefix prefix e =
   e /@ (fun elt -> prefix, elt)

let enum_merge2i cmp e =
   e |> Enum.mapi enum_add_prefix |> enum_merge2 cmp

(* vim: set ts=3 sw=3 tw=80 : *)
