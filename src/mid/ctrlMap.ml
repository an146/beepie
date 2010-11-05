open MidiTypes

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

let x = IntMap.empty;;

let create ?(min = 0) ?(max = 127) default =
    {
        min = min;
        max = max;
        default = default;
        map = x
    };;
