open GdkKeysyms
open GMain

class menu packing =
   object (self)
      val menubar = GMenu.menu_bar ~packing ()
      val mutable m_output_device = None

      method refresh_devices () =
         let menu =
            match m_output_device with
               None -> raise (Failure "menu not constructed")
            |  Some m -> m
         in
         List.iter (fun c -> c#destroy ()) menu#all_children;

         let factory = new GMenu.factory menu in
         let devices = Midiio.enum_output_devices () in
         let have_active = ref false in
         let create_item device group =
            let id = device.Midiio.id in
            let name = device.Midiio.name in
            let callback = fun set -> if set then Midiio.set_output_device id in
            let label = id ^ "    " ^ name in
            let active = (Midiio.get_output_device () = id) in
            if active then have_active := true;
            factory#add_radio_item label ~active ~callback ~group
         in
         let first = create_item (List.hd devices) None in
         let create_item device = create_item device first#group in
         let _ = List.map create_item (List.tl devices) in
         if not !have_active then begin
            first#set_active true;
            Midiio.set_output_device (List.hd devices).Midiio.id
         end

      initializer
         let factory = new GMenu.factory menubar in
         let accel_group = factory#accel_group in

         (* top-level *)
         let m_file = factory#add_submenu "File" in
         let m_settings = factory#add_submenu "Settings" in

         (* File *)
         let factory = new GMenu.factory m_file ~accel_group in
         let _ = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

         (* Settings *)
         let factory = new GMenu.factory m_settings ~accel_group in
         m_output_device <- Some (factory#add_submenu "Output device");
         let callback = self#refresh_devices in
         let _ = factory#add_item "Refresh devices" ~callback in
         let callback = fun () -> Midiio.output_note 0 60 1.0 in
         let _ = factory#add_item "Test sound" ~callback in
         ()
   end;;

(* vim: set ts=3 sw=3 tw=80 : *)
