open Batteries

type 'a t = {
   heap : 'a DynArray.t;
   cmp : 'a -> 'a -> int;
}

(* Exported *)
let length {heap} = DynArray.length heap

(* Internal *)
let get {heap} i = DynArray.unsafe_get heap i
let set {heap} i x = DynArray.unsafe_set heap i x

let swap heap i j =
   let tmp = get heap i in
   set heap i (get heap j);
   set heap j tmp

let rec reorder_down h i =
   let childs = i * 2 + 1 in
   if childs >= length h then
      ()
   else (
      let c =
         if childs + 1 >= length h then
            childs
         else if h.cmp (get h childs) (get h (childs + 1)) < 0 then
            childs
         else
            childs + 1
      in
      if h.cmp (get h i) (get h c) < 0 then
         ()
      else (
         swap h i c;
         reorder_down h c
      )
   )

(* Exported *)
let make cmp n = {heap = DynArray.make n; cmp}
let is_empty {heap} = DynArray.empty heap

let top {heap} = DynArray.get heap 0

let reorder_top h = reorder_down h 0

let push h x =
   let l = length h in
   DynArray.add h.heap x;
   let par i = (i - 1) / 2 in
   let rec fix_heap = function
      | i when h.cmp (get h i) (get h (par i)) < 0 ->
            swap h i (par i);
            fix_heap (par i)
      | _ -> ()
   in fix_heap l

let drop h =
   if is_empty h then failwith "pop: heap is empty";
   set h 0 (get h (length h - 1));
   DynArray.delete_last h.heap;
   reorder_top h

let pop h =
   let ret = get h 0 in
   drop h;
   ret

let from_enum cmp e =
   let rebuild_heap h =
      for i = (length h) / 2 - 1 downto 0 do
         reorder_down h i
      done
   in
   let h = {heap = DynArray.of_enum e; cmp} in
   rebuild_heap h;
   h

(* vim: set ts=3 sw=3 tw=80 : *)
