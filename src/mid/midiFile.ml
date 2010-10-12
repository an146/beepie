open IntX
open MidiCmd

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

class file (_division : int) =
   object (self)
      val division_ = _division
      val mutable filename_ = ""
      val mutable tracks_ : track array = [| |]

      method division = division_
      method filename = filename_
      method set_filename _filename = filename_ <- _filename
      method export fn =
         Export.export self fn;
         self#set_filename fn
      method add_track () = tracks_ <- Array.append tracks_ [| empty_track |]
      method tracks = tracks_
      method track i = tracks_.(i)
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
