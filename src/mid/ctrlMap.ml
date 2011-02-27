open Batteries

exception Out_of_range

type 'a t = {
   min : 'a option;
   max : 'a option;
   default : 'a;
   map : (int, 'a) PMap.t
};;

let create ?min ?max default =
   {
      min = min;
      max = max;
      default = default;
      map = PMap.empty
   };;

let end_value map default =
   if PMap.is_empty map then
      default
   else
      let _, v = PMap.max_binding map in
      v;;

let get time ctrlmap =
   (* TODO: avoid using IntMap.split;
    * write own Map impl with lower_bound *)
   let lower, _, _ = PMap.split (time + 1) ctrlmap.map in
   end_value lower ctrlmap.default;;

let set time value ctrlmap =
   match ctrlmap.min, ctrlmap.max with
   | Some min, _ when value < min -> raise Out_of_range
   | _, Some max when value > max -> raise Out_of_range
   | _, _ -> ();
   let lower, _, upper = PMap.split time ctrlmap.map in
   let ctrlmap =
      if value = end_value lower ctrlmap.default then
         {ctrlmap with map = PMap.remove time ctrlmap.map}
      else
         {ctrlmap with map = PMap.add time value ctrlmap.map}
   in
   if PMap.is_empty upper then
      ctrlmap
   else
      let k, v = PMap.min_binding upper in
      if value = v then
         {ctrlmap with map = PMap.remove k ctrlmap.map}
      else
         ctrlmap

let enum ctrlmap = PMap.enum ctrlmap.map;;

let bindings ctrlmap = ctrlmap |> enum |> List.of_enum;;

let clear ctrlmap = {ctrlmap with map = PMap.empty}
let reset v ctrlmap = ctrlmap |> clear |> set 0 v
(* vim: set ts=3 sw=3 tw=80 : *)
