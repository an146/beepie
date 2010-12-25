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

type track = { id : int; name : string; notes : note PSet.t }

let check_ctrltype t =
   if not (Ctrl.is_supported t) then
      let name = Ctrl.name t in
      invalid_arg ("Unsupported controller: " ^ name);;

class channel (id : int option) =
   object (self)
      val mutable owner = None
      val mutable ctrls_ =
         let entry c = c, Ctrl.create_map c in
         Ctrl.all_supported |> List.enum |> Enum.map entry |> PMap.of_enum

      method id = id
      method ctrl ctrltype =
         check_ctrltype ctrltype;
         try PMap.find ctrltype ctrls_
         with Not_found ->
            let n = Enum.count self#enum_ctrls in
            invalid_arg ((string_of_int n) ^ (Ctrl.name ctrltype))

      method set_ctrl ctrltype ctrl = ctrls_ <- PMap.add ctrltype ctrl ctrls_
      method enum_ctrls = (PMap.enum ctrls_)
      method owner = owner
      method set_owner (o : int option) = owner <- o

      initializer
         match id with
         | Some id when id < 0 || id >= 16 -> raise Out_of_range
         | _ -> ()
   end;;

class file ?(tracks_count = 0) (_division : int) =
   object (self)
      val mutable filename_ = ""
      val mutable tracks = [| |]

      val mutable channels =
         let create_channel i = new channel (Some i) in
         Array.init 16 create_channel

      method division = _division
      method filename = filename_
      method set_filename _filename = filename_ <- _filename
      method add_track =
         let id = Array.length tracks in
         let name = "" in
         let notes = PSet.create note_compare in
         tracks <- Array.append tracks [| {id; name; notes} |]

      method track i = tracks.(i)
      method enum_tracks = tracks |> Array.enum
      method enumi_tracks =
         let last = Array.length tracks - 1 in
         (0 -- last) /@ (fun i -> i, tracks.(i))

      method channel i = channels.(i)
      method channels = channels
      method enum_channels = channels |> Array.enum
      method channel_owner i = (self#channel i)#owner

      method insert_note track note =
         let old_track = self#track track in
         let new_track = {
            id = old_track.id;
            name = old_track.name;
            notes = PSet.add note old_track.notes;
         } in
         tracks.(track) <- new_track;
         (self#channel note.channel)#set_owner (Some new_track.id)

      initializer
         for i = 1 to tracks_count do
            self#add_track
         done
   end;;

(* vim: set ts=3 sw=3 tw=80 : *)
