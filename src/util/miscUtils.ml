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

let global_init g f =
   if not (Global.isdef g) then
      Global.set g (f ());
   Global.get g

(** finds such x, i <= x <= j for which
 *  f a is false for all a < x
 *  and true for all a >= x,
 *  assuming that it exists *)
let rec binary_search f i j =
   if i >= j then (
      if f i then
         i
      else
         i + 1
   ) else (
      let k = (i + j) / 2 in
      if f k then
         binary_search f i (k - 1)
      else
         binary_search f (k + 1) j
   )

(* vim: set ts=3 sw=3 tw=80 : *)
