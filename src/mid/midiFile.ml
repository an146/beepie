open Batteries
open MidiCmd
open MidiNote

exception Channel_conflict

type track = {
   notes : (int * note) PSet.t;
}

type file = {
   division : int;
   tracks : track array;
   channel_usage : (int * int) option array;
   ctrlmaps : ((int * Ctrl.t), int CtrlMap.t) PMap.t;
}

let note_compare (c1, n1) (c2, n2) =
   let values c n = [n.on_time; n.off_time; n.midipitch; c] in
   compare (values c1 n1) (values c2 n2)

let default_ctrlmaps =
   let entry (ch, ct) = (ch, ct), Ctrl.create_map ct in
   Ctrl.all_supported_c |> List.enum |> Enum.map entry |> PMap.of_enum

let create division = {
   division;
   tracks = [| |];
   channel_usage = Array.make 16 None;
   ctrlmaps = default_ctrlmaps;
}

let inc_usage usage c t =
   let new_usage =
      match usage.(c) with
      | None -> Some (t, 1)
      | Some (t', u) -> if t != t' then raise Channel_conflict; Some (t, u + 1)
   in
   usage.(c) <- new_usage

let division {division} = division

let add_track f =
   let track = {notes = PSet.create note_compare} in
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
   let channel_usage = Array.copy f.channel_usage in
   inc_usage channel_usage c t;
   let notes = PSet.add (c, note) track.notes in
   tracks.(t) <- {notes};
   {f with tracks; channel_usage}

let channel_owner c f =
   match f.channel_usage.(c) with
   | Some (o, _) -> Some o
   | None -> None

let check_ctrlmap_idx (ch, ct) =
   if ch < 0 || ch >= 16 then
      invalid_arg "invalid channel idx";
   if not (Ctrl.is_supported ct) then
      invalid_arg ((Ctrl.name ct) ^ ": unsupported")

let ctrlmap idx f =
   check_ctrlmap_idx idx;
   PMap.find idx f.ctrlmaps

let set_ctrlmap idx map f =
   check_ctrlmap_idx idx;
   {f with ctrlmaps = PMap.add idx map f.ctrlmaps}

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
   let ctrlmap = ctrlmap
   let set_ctrlmap = set_ctrlmap
   let enum_notes = enum_notes
   let tracks_count = tracks_count
   let tracks = tracks
end

(* vim: set ts=3 sw=3 tw=80 : *)
