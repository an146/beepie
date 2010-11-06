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

let get time ctrlmap =
   (* TODO: avoid using IntMap.split;
    * write own Map impl with lower_bound *)
   let less, _, _ = IntMap.split (time + 1) ctrlmap.map in
   try
      let _, v = IntMap.max_binding less in
      v
   with Not_found -> ctrlmap.default;;

let set time value ctrlmap =
   if value < ctrlmap.min || value > ctrlmap.max then
      raise Out_of_range;
   {
      min = ctrlmap.min;
      max = ctrlmap.max;
      default = ctrlmap.default;
      map = IntMap.add time value ctrlmap.map
   };;

let bindings ctrlmap = IntMap.bindings ctrlmap.map;;

(* vim: set ts=3 sw=3 tw=80 : *)
