open Batteries
open MidiNote

type t = {
   notes : (int * note) PSet.t;
   channel_usage : (int * int) list;
}

let note_compare (c1, n1) (c2, n2) =
   let values c n = [n.on_time; n.off_time; n.midipitch; c] in
   compare (values c1 n1) (values c2 n2)

let inc_usage c track =
   let usage = track.channel_usage in
   let u =
      try List.assoc c usage
      with Not_found -> 0
   in
   let channel_usage = usage |> List.remove_assoc c |> List.cons (c, u + 1) in
   {track with channel_usage}

let create () = {notes = PSet.create note_compare; channel_usage = []}

let add_note note c track =
   let track = inc_usage c track in
   {track with notes = PSet.add (c, note) track.notes}

let owns c {channel_usage} =
   List.mem_assoc c channel_usage

let enum {notes} = PSet.enum notes

(* vim: set ts=3 sw=3 tw=80 : *)
