open IntX
open MidiCmd
open MidiTypes

exception Out_of_range

type note_end = miditime * velocity
type note = channel * midipitch * note_end * note_end

let note_compare (_, Int7 p1, (beg1, _), (end1, _))
                 (_, Int7 p2, (beg2, _), (end2, _)) =
   if beg1 != beg2 then compare beg1 beg2
   else if end1 != end2 then compare end1 end2
   else compare p1 p2;;

module Note =
   struct
      type t = note
      let compare = note_compare
   end

module NoteSet = Set.Make(Note)

type track = { name : string; notes : NoteSet.t }
let empty_track = { name = ""; notes = NoteSet.empty };;

class channel (_id : int option) =
   object (self)
      val mutable ctrls_ =
         let create_map c () = Ctrl.create_map c in
         PMap.mapi create_map Ctrl.all_supported

      method id = _id
      method ctrl ctrltype = PMap.find ctrltype ctrls_
      method set_ctrl ctrltype ctrl = ctrls_ <- PMap.add ctrltype ctrl ctrls_

      initializer
         let check_id id =
            if id < 0 || id >= 16 then
               raise Out_of_range
         in
         Option.may check_id _id
   end;;

class file ?(tracks_count = 0) (_division : int) =
   object (self)
      val mutable filename_ = ""
      val mutable tracks_ = Array.create tracks_count empty_track
      val mutable channels_ =
         let create_channel i = new channel (Some i) in
         Array.init 16 create_channel

      method division = _division
      method filename = filename_
      method set_filename _filename = filename_ <- _filename
      method add_track () = tracks_ <- Array.append tracks_ [| empty_track |]
      method tracks = tracks_
      method track i = tracks_.(i)
      method channel i = channels_.(i)

      method insert_note t c n (on_time, on_vel) (off_time, off_vel) =
         let note = (c, n, (on_time, on_vel), (off_time, off_vel)) in
         let old_track = self#track t in
         let new_track = {
            name = old_track.name;
            notes = NoteSet.add note old_track.notes
         } in
         tracks_.(t) <- new_track
   end;;

(* vim: set ts=3 sw=3 tw=80 : *)
