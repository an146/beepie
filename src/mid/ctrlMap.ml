open BatPervasives
open MidiTypes

exception Out_of_range

type t = {
   min : int;
   max : int;
   default : int;
   map : (miditime, int) BatPMap.t
};;

let create ?(min = 0) ?(max = 127) default =
   {
      min = min;
      max = max;
      default = default;
      map = BatPMap.empty
   };;

let end_value map default =
   if BatPMap.is_empty map then
      default
   else
      let _, v = BatPMap.max_binding map in
      v;;

let get time ctrlmap =
   (* TODO: avoid using IntMap.split;
    * write own Map impl with lower_bound *)
   let lower, _, _ = BatPMap.split (time + 1) ctrlmap.map in
   end_value lower ctrlmap.default;;

let set_map ctrlmap map =
   {
      min = ctrlmap.min;
      max = ctrlmap.max;
      default = ctrlmap.default;
      map = map
   };;

let set time value ctrlmap =
   if value < ctrlmap.min || value > ctrlmap.max then
      raise Out_of_range
   else
      let lower, _, upper = BatPMap.split time ctrlmap.map in
      let ctrlmap =
         if value = end_value lower ctrlmap.default then
            set_map ctrlmap (BatPMap.remove time ctrlmap.map)
         else
            set_map ctrlmap (BatPMap.add time value ctrlmap.map)
      in
      if BatPMap.is_empty upper then
         ctrlmap
      else
         let k, v = BatPMap.min_binding upper in
         if value = v then
            set_map ctrlmap (BatPMap.remove k ctrlmap.map)
         else
            ctrlmap;;

let enum ctrlmap = BatPMap.enum ctrlmap.map;;

let bindings ctrlmap = ctrlmap |> enum |> BatList.of_enum;;

(* vim: set ts=3 sw=3 tw=80 : *)
