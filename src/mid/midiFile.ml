open Batteries
open MidiNote
module Track = MidiTrack

exception Channel_conflict

type t = {
   division : int;
   tracks : Track.t list;
   ctrl_maps : ((int * Ctrl.t), int CtrlMap.t) PMap.t;
   tempo_map : int CtrlMap.t;
   timesig_map : TimeSig.t CtrlMap.t;
}

let default_ctrl_maps =
   let entry (ch, ct) = (ch, ct), Ctrl.create_map ct in
   Ctrl.all_supported_c |> List.enum |> Enum.map entry |> PMap.of_enum

let create division = {
   division;
   tracks = [];
   ctrl_maps = default_ctrl_maps;
   tempo_map = CtrlMap.create ~min:0 ~max:0xFFFFFF (60000000 / 120);
   timesig_map = CtrlMap.create (TimeSig.create 4 4);
}

let division {division} = division

let tracks f = List.enum f.tracks
let set_tracks tracks f = {f with tracks = List.of_enum tracks}
let tracks_count {tracks} = List.length tracks
let track i f = List.at f.tracks i

let add_track f =
   let track = Track.create () in
   let tracks = List.append f.tracks [track] in
   {f with tracks}

let remove_track i f =
   let tracks = tracks f |> DynArray.of_enum in
   DynArray.delete tracks i;
   set_tracks (DynArray.enum tracks) f

let map_ith i f = List.mapi (fun j elt -> if i = j then f elt else elt)

let map_track i fn f =
   let tracks = map_ith i fn f.tracks in
   {f with tracks}

let add_note ?channel t note f =
   let c =
      match channel with
      | Some c -> c
      | None -> assert false
   in
   map_track t (Track.add_note note c) f

let channel_owner c f =
   let tracks = tracks f |> Array.of_enum in
   Array.Exceptionless.findi (Track.owns c) tracks

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

let tvalue (tn, ct) f =
   track tn f |> Track.tvalue ct

let set_tvalue (tn, ct) v f =
   let f = map_track tn (Track.reset_tvalue ct v) f in
   let fix_channel f c =
      let ctrl_maps = f.ctrl_maps |> PMap.modify (c, ct) (CtrlMap.reset v) in
      {f with ctrl_maps}
   in
   Track.channels (track tn f) |> Enum.fold fix_channel f

let set_volume tn v f = set_tvalue (tn, Ctrl.volume) v f

let get_note_ctrl (chan, note) ct f =
   let cm = ctrl_map (chan, ct) f in
   CtrlMap.get note.on_time cm

let reset_tvalues f =
   let do_reset track =
      try
         let note = Track.choose_note track in
         let tvalues =
            Ctrl.tvalues |> List.enum |> Enum.map (fun ct ->
               ct, get_note_ctrl note ct f
            ) |> PMap.of_enum
         in
         Track.reset_tvalues tvalues track
      with _ -> track
   in
   {f with tracks = List.map do_reset f.tracks}

(* vim: set ts=3 sw=3 tw=80 : *)
