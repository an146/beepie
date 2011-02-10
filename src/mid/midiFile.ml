open Batteries
open MidiNote

exception Channel_conflict

type track = {
   notes : (int * note) PSet.t;
   channel_usage : (int * int) list;
}

type file = {
   division : int;
   tracks : track array;
   ctrl_maps : ((int * Ctrl.t), int CtrlMap.t) PMap.t;
   tempo_map : int CtrlMap.t;
   timesig_map : TimeSig.t CtrlMap.t;
}

let note_compare (c1, n1) (c2, n2) =
   let values c n = [n.on_time; n.off_time; n.midipitch; c] in
   compare (values c1 n1) (values c2 n2)

let default_ctrl_maps =
   let entry (ch, ct) = (ch, ct), Ctrl.create_map ct in
   Ctrl.all_supported_c |> List.enum |> Enum.map entry |> PMap.of_enum

let create division = {
   division;
   tracks = [| |];
   ctrl_maps = default_ctrl_maps;
   tempo_map = CtrlMap.create ~min:0 ~max:0xFFFFFF (60000000 / 120);
   timesig_map = CtrlMap.create (TimeSig.create 4 4);
}

let inc_usage track c =
   let usage = track.channel_usage in
   let u =
      try List.assoc c usage
      with Not_found -> 0
   in
   let channel_usage = usage |> List.remove_assoc c |> List.cons (c, u + 1) in
   {track with channel_usage}

let division {division} = division

let add_track f =
   let track = {notes = PSet.create note_compare; channel_usage = []} in
   let tracks = Array.append f.tracks [| track |] in
   {f with tracks}

let add_note ?channel t note f =
   let tracks = Array.copy f.tracks in
   let track = tracks.(t) in
   let c =
      match channel with
      | Some c -> c
      | None -> assert false
   in
   let track = inc_usage track c in
   let notes = PSet.add (c, note) track.notes in
   tracks.(t) <- {track with notes};
   {f with tracks}

let channel_owner c {tracks} =
   let owns t = List.mem_assoc c t.channel_usage in
   Array.Exceptionless.findi owns tracks

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

let enum_notes ?track f =
   match track with
   | Some t -> PSet.enum f.tracks.(t).notes
   | None -> assert false

let tracks_count {tracks} = Array.length tracks

let tracks f = 0 -- (tracks_count f - 1)

module File = struct
   type t = file

   let create = create
   let division = division
   let add_track = add_track
   let add_note = add_note
   let channel_owner = channel_owner
   let ctrl_map = ctrl_map
   let set_ctrl_map = set_ctrl_map
   let tempo_map = tempo_map
   let set_tempo_map = set_tempo_map
   let timesig_map = timesig_map
   let set_timesig_map = set_timesig_map
   let enum_notes = enum_notes
   let tracks_count = tracks_count
   let tracks = tracks
end

(* vim: set ts=3 sw=3 tw=80 : *)
