open MidiCmd

exception Out_of_range

type note =
   {
      channel    : int;
      midipitch  : int;
      on_time    : int;
      on_vel     : int;
      off_time   : int;
      off_vel    : int;
   }

let note_compare n1 n2 =
   let values n = [ n.on_time; n.off_time; n.midipitch; n.channel ] in
   compare (values n1) (values n2)

type track = { name : string; notes : note PSet.t }
let empty_track = { name = ""; notes = PSet.create note_compare };;

let check_ctrltype t =
   if not (Ctrl.is_supported t) then
      let name = Ctrl.name t in
      invalid_arg ("Unsupported controller: " ^ name);;

class channel (_id : int option) =
   object (self)
      val mutable ctrls_ =
         let e = Ctrl.all_supported () /@ fun c -> c, Ctrl.create_map c in
         PMap.of_enum e

      method id = _id
      method ctrl ctrltype =
         check_ctrltype ctrltype;
         try PMap.find ctrltype ctrls_
         with Not_found ->
            let n = Enum.count self#enum_ctrls in
            invalid_arg ((string_of_int n) ^ (Ctrl.name ctrltype))

      method set_ctrl ctrltype ctrl = ctrls_ <- PMap.add ctrltype ctrl ctrls_
      method enum_ctrls = (PMap.enum ctrls_)

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
      method add_track = tracks_ <- Array.append tracks_ [| empty_track |]
      method tracks = tracks_
      method track i = tracks_.(i)
      method channel i = channels_.(i)
      method enum_channels =
         (Array.range channels_) /@ (fun i -> i, self#channel i)

      method insert_note t c n (on_time, on_vel) (off_time, off_vel) =
         let note = {
            channel = c;
            midipitch = n;
            on_time;
            on_vel;
            off_time;
            off_vel;
         } in
         let old_track = self#track t in
         let new_track = {
            name = old_track.name;
            notes = PSet.add note old_track.notes
         } in
         tracks_.(t) <- new_track
   end;;

(* vim: set ts=3 sw=3 tw=80 : *)
