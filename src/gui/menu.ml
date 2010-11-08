open GdkKeysyms
open GMain

module type MainWindow_sig =
   sig
      val window : GWindow.window
      val add_file : MidiFile.file -> unit
      val set_status : string -> unit
   end

module Make (MainWindow : MainWindow_sig) = struct
   let profile f =
      let start_time = Unix.time () in
      f ();
      let time = Unix.time () -. start_time in
      if time >= 0.1 then
         MainWindow.set_status (Printf.sprintf "Done (%.3f s)" time)
      else
         MainWindow.set_status "Done";;

   let new_file () =
      MainWindow.add_file (new MidiFile.file 240)

   let open_files () =
      let filenames = FileDialog.get_open_filenames (MainWindow.window) in
      let open_file fn = MainWindow.add_file (Import.import_file fn) in
      profile (fun () -> List.iter open_file filenames)

   let do_test_sound () =
      MidiIo.output_note 0 60 1.0;
      Printf.printf "bla\n";
      flush stdout

   let test_sound () =
      let _ = Thread.create do_test_sound () in
      Printf.printf "xyi\n";
      flush stdout

   class menu packing =
      object (self)
         val menubar = GMenu.menu_bar ~packing ()
         val mutable m_output_device = None

         method refresh_devices () =
            let menu =
               match m_output_device with
                 None -> failwith "menu not constructed"
               | Some m -> m
            in
            List.iter (fun c -> c#destroy ()) menu#all_children;

            let factory = new GMenu.factory menu in
            let devices = MidiIo.enum_output_devices () in
            let have_active = ref false in
            let create_item device group =
               let id = device.MidiIo.id in
               let name = device.MidiIo.name in
               let callback = fun set -> if set then MidiIo.set_output_device id in
               let label = id ^ "    " ^ name in
               let active = (MidiIo.get_output_device () = id) in
               if active then have_active := true;
               factory#add_radio_item label ~active ~callback ~group
            in
            let first = create_item (List.hd devices) None in
            let create_item device = create_item device first#group in
            let _ = List.map create_item (List.tl devices) in
            if not !have_active then begin
               first#set_active true;
               MidiIo.set_output_device (List.hd devices).MidiIo.id
            end

         initializer
            let factory = new GMenu.factory menubar in
            let accel_group = factory#accel_group in

            let wrap_errors f () =
               MainWindow.set_status "";
               try f () with
                   Failure desc -> MainWindow.set_status ("Error: " ^ desc)
                 | _ -> MainWindow.set_status "Unhandled exception occurred"
            in

            let add_item (factory : GMenu.menu GMenu.factory) callback =
               factory#add_item ~callback: (wrap_errors callback)
            in

            (* File *)
            let _ =
               let m_file = factory#add_submenu "File" in
               let factory = new GMenu.factory m_file ~accel_group in
               let add_item = add_item factory in
               let _ = add_item new_file   "New"     ~key:_N in
               let _ = add_item open_files "Open..." ~key:_O in
               let _ = add_item Main.quit  "Quit"    ~key:_Q in ()
            in

            (* Settings *)
            let _ =
               let m_settings = factory#add_submenu "Settings" in
               let factory = new GMenu.factory m_settings ~accel_group in
               let add_item = add_item factory in
               m_output_device <- Some (factory#add_submenu "Output device");
               let _ = add_item self#refresh_devices "Refresh devices" in
               let _ = add_item test_sound "Test sound" in ()
            in ()
      end
end

(* vim: set ts=3 sw=3 tw=80 : *)
