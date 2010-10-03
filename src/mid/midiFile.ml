open MidiCmd

type note_end = miditime * velocity
type note = channel * midipitch * note_end * note_end

let note_compare (_, p1, (beg1, _), (end1, _)) (_, p2, (beg2, _), (end2, _)) =
   if beg1 != beg2 then compare beg1 beg2
   else if end1 != end2 then compare end1 end2
   else compare p1 p2;;

module Note =
   struct
      type t = note
      let compare = note_compare
   end

module NoteSet = Set.Make(Note)

class track =
   object (self)
      val mutable name_ = ""
      val mutable notes_ = NoteSet.empty

      method name = name_
      method notes = notes_
   end

class file (_division : int) =
   object (self)
      val division_ = _division
      val mutable filename_ = ""
      val mutable tracks_ : track list = []

      method division = division_
      method filename = filename_
      method set_filename _filename = filename_ <- _filename
      method export fn =
         Export.export self fn;
         self#set_filename fn
      method add_track () =
         let ret = new track in
         tracks_ <- tracks_ @ [ret];
         ret
      method tracks = tracks_
   end;;

(* vim: set ts=3 sw=3 tw=80 : *)
