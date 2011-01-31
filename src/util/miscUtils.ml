open Batteries

let enum_merge2 cmp e =
   let heap =
      let cmp_e a b =
         match Enum.peek a, Enum.peek b with
         | None, _ -> 1
         | _, None -> -1
         | Some x, Some y -> cmp x y
      in
      BinaryHeap.from_enum cmp_e e
   in
   let get_element () =
      let e = BinaryHeap.top heap in
      let elt = Enum.get e in
      BinaryHeap.reorder_top heap;
      elt
   in
   Enum.from_while get_element

let enum_add_prefix prefix e =
   e /@ (fun elt -> prefix, elt)

let enum_merge2i cmp e =
   e |> Enum.mapi enum_add_prefix |> enum_merge2 cmp

(* vim: set ts=3 sw=3 tw=80 : *)
