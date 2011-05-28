open Batteries
open MidiFile
open TabRender
open TabX.Infix

type t = {
   mutable scope : (F.t * track_id list) option;
   width_cache : TabX.t DynArray.t;
}

let create () = {
   scope = None;
   width_cache = DynArray.create ();
}

exception Nothing_to_do

let do_refresh layout scope =
   if not (
      layout.scope != Some scope
      || DynArray.empty layout.width_cache
   ) then raise Nothing_to_do;
   layout.scope <- Some scope;
   DynArray.clear layout.width_cache;
   let (f, tracks) = scope in
   F.measures f |> Vect.enum |> Enum.map (fun m ->
      let w =
         let get_endpoint elt =
            let len = TabX.text elt.text in
            elt.x +: len
         in
         render_measure f tracks m
         |> Enum.map get_endpoint
         |> Enum.fold TabX.max TabX.zero
      in
      (* empty measure contains 1 empty column *)
      let w = if w > TabX.zero then w else w +: TabX.char in
      (* measure delimiting space *)
      w +: TabX.spacesi 2
   ) |> Enum.iter (DynArray.add layout.width_cache)

let refresh layout file tracks =
   try do_refresh layout (file, tracks)
   with Nothing_to_do -> ()

let measure_width layout i = DynArray.get layout.width_cache i

let enum_rows layout width =
   let mwidth i = measure_width layout i in
   let i = ref 0 in
   Enum.from (fun () ->
      let q = Queue.create () in
      let push () =
         Queue.push !i q;
         incr i;
      in
      (try
         let s = ref (mwidth !i) in
         push ();
         while true do
            let s' = !s +: mwidth !i in
            let penalty s =
               let ssize = TabX.space_size s width in
               Float.abs (1.3 -. ssize)
               +. (if ssize < 0.9 then 10.0 else 0.0)
            in
            if penalty s' > penalty !s then
               failwith "it's enough";
            s := s';
            push ()
         done
      with _ -> ());
      if Queue.is_empty q then
         raise Enum.No_more_elements
      else
         Queue.enum q
   )

let row_width layout row =
   Enum.clone row |> Enum.map (measure_width layout) |> Enum.reduce (+:)

(* vim: set ts=3 sw=3 tw=80 : *)
