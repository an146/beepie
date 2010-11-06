open MidiTypes

exception Out_of_range

module IntMap = Map.Make (struct
   type t = miditime
   let compare = compare
end)

type t = {
   min : int;
   max : int;
   default : int;
   map : int IntMap.t
};;

let create ?(min = 0) ?(max = 127) default =
   {
      min = min;
      max = max;
      default = default;
      map = IntMap.empty
   };;

let end_value map default =
   try
      let _, v = IntMap.max_binding map in
      v
   with Not_found -> default;;

let get time ctrlmap =
   (* TODO: avoid using IntMap.split;
    * write own Map impl with lower_bound *)
   let lower, _, _ = IntMap.split (time + 1) ctrlmap.map in
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
      let lower, _, upper = IntMap.split time ctrlmap.map in
      let ctrlmap =
         if value = end_value lower ctrlmap.default then
            set_map ctrlmap (IntMap.remove time ctrlmap.map)
         else
            set_map ctrlmap (IntMap.add time value ctrlmap.map)
      in
      if IntMap.is_empty upper then
         ctrlmap
      else
         let k, v = IntMap.min_binding upper in
         if value = v then
            set_map ctrlmap (IntMap.remove k ctrlmap.map)
         else
            ctrlmap;;

let bindings ctrlmap = IntMap.bindings ctrlmap.map;;

(* vim: set ts=3 sw=3 tw=80 : *)
