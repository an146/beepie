open Batteries
open MidiNote
open PMap.Infix

exception Channel_conflict

type track_id = int

type measure = {
   start : int;
   len : int;
   notes : (int * note) list;
}

type t = {
   division : int;
   tracks : track_id Vect.t;
   channel_usage : (track_id * int) Vect.t;
   measures : measure Vect.t;

   tvalues : ((track_id * Ctrl.t), int) PMap.t;
   ctrl_maps : ((int * Ctrl.t), int CtrlMap.t) PMap.t;
   tempo_map : int CtrlMap.t;
   timesig_map : TimeSig.t CtrlMap.t;
}

let create division =
   let ctrl_maps =
      let entry (ch, ct) = (ch, ct), Ctrl.create_map ct in
      Ctrl.all_supported_c |> List.enum |> Enum.map entry |> PMap.of_enum
   in
   {
      division;
      tracks = Vect.empty;
      channel_usage = Vect.make 16 (-1, 0);
      measures = Vect.empty;

      tvalues = PMap.empty;
      ctrl_maps;
      tempo_map = CtrlMap.create ~min:0 ~max:0xFFFFFF (60000000 / 120);
      timesig_map = CtrlMap.create (TimeSig.create 4 4);
   }

let division {division} = division

let track i {tracks} = Vect.get tracks i
let tracks {tracks} = Vect.enum tracks
let tracks_count {tracks} = Vect.length tracks
let track_index ({tracks}, tr) = (Vect.findi ((=) tr) tracks - 1)

let owns {channel_usage} tr c =
   let (tr', n) = Vect.get channel_usage c in
   tr = tr' && n > 0

let channels tr f =
   (0 -- 15) |> Enum.filter (owns f tr)

let add_track =
   let id = ref 1000 in
   fun f ->
      let id = (incr id; !id) in
      {f with tracks = Vect.append id f.tracks}

let remove_track tr f =
   assert (Enum.is_empty (channels tr f));
   {f with tracks = Vect.filter ((<>) tr) f.tracks}

let channel_owner c {channel_usage} =
   let (t, n) = Vect.get channel_usage c in
   if n > 0 then Some t else None

let check_ctrl_idx (ch, ct) =
   if ch < 0 || ch >= 16 then
      invalid_arg "invalid channel idx";
   if not (Ctrl.is_supported ct) then
      invalid_arg ((Ctrl.name ct) ^ ": unsupported")

let ctrl_map idx f =
   check_ctrl_idx idx;
   PMap.find idx f.ctrl_maps

let set_ctrl_map idx map f =
   check_ctrl_idx idx;
   {f with ctrl_maps = PMap.add idx map f.ctrl_maps}

let tempo_map {tempo_map} = tempo_map
let set_tempo_map tempo_map f = {f with tempo_map}

let timesig_map {timesig_map} = timesig_map
let set_timesig_map timesig_map f = {f with timesig_map}

let tvalue ct ({tvalues}, tr) = tvalues --> (tr, ct)

let set_tvalue ct v (f, tr) =
   let f = {f with tvalues = f.tvalues <-- ((tr, ct), v)} in
   let fix_channel f c =
      let map = Ctrl.create_map ct |> CtrlMap.reset v in
      {f with ctrl_maps = f.ctrl_maps <-- ((c, ct), map)}
   in
   channels tr f |> Enum.fold fix_channel f

let volume = tvalue Ctrl.volume
let set_volume = set_tvalue Ctrl.volume

let find_measure t measures =
   MiscUtils.binary_search (fun i ->
      let {start; len} = Vect.get measures i in
      t < start + len
   ) 0 (Vect.length measures)

let add_note ?channel tr n f =
   let c =
      match channel with
      | Some c -> c
      | None -> assert false
   in
   let channel_usage =
      Vect.modify f.channel_usage c (function
         | _, 0 ->
               tr, 1
         | tr', n ->
               if tr != tr' then
                  failwith "channel conflict";
               (tr, n + 1)
      )
   in
   let measures =
      let m = ref f.measures in
      let t = ref (try (Vect.last !m).start with _ -> 0) in
      while !t < n.off_time do
         let l = CtrlMap.get !t f.timesig_map |> TimeSig.len f.division in
         m := Vect.append {start = !t; len = l; notes = []} !m;
         t := !t + l
      done;
      !m
   in
   let measures =
      let s = find_measure n.on_time measures in
      let e = find_measure (n.off_time - 1) measures in
      let add m =
         let note_compare (c1, n1) (c2, n2) =
            let values c n = [n.on_time; n.off_time; n.midipitch; c] in
            compare (values c1 n1) (values c2 n2)
         in
         assert (m.start < n.off_time && m.start + m.len >= n.on_time);
         let notes = List.sort ~cmp:note_compare ((c, n) :: m.notes) in
         {m with notes}
      in
      Enum.fold (fun ms i -> Vect.modify ms i add) measures (s -- e)
   in
   let tvalues =
      try
         List.fold_left (fun tv ct ->
            if PMap.mem (tr, ct) tv then
               failwith "already have tvalues; nothing to do";
            let cm = ctrl_map (c, ct) f in
            PMap.add (tr, ct) (CtrlMap.get n.on_time cm) tv
         ) f.tvalues Ctrl.tvalues
      with _ -> f.tvalues
   in
   {f with channel_usage; measures; tvalues}

let enum_notes ?track f =
   let e =
      let notes m =
         List.enum m.notes // (fun (_, n) -> n.on_time >= m.start)
      in
      Vect.enum f.measures |> Enum.map notes |> Enum.flatten
   in
   match track with
   | Some t -> e |> Enum.filter (fun (c, _) -> owns f t c)
   | None -> e

(* vim: set ts=3 sw=3 tw=80 : *)
