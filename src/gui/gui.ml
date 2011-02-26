open Batteries
open FileWidget
open GdkKeysyms
open GtkSugar
open MiscUtils (* global_init *)
open React

let g_window = Global.empty "window"
and g_statusbar = Global.empty "statusbar"

let set_status =
   let ctx = Global.empty "status_ctx" in
   fun s -> (
      let ctx = global_init ctx (fun () ->
         (Global.get g_statusbar)#new_context ~name:"Status"
      ) in
      ctx#pop ();
      ignore (ctx#push s)
   )

let profile f =
   let start_time = Unix.time () in
   f ();
   let time = Unix.time () -. start_time in
   if time >= 0.1 then
      set_status (Printf.sprintf "Done (%.3f s)" time)
   else
      set_status "Done";;

let create_main_window () =
   let (files : file_widget tnotebook Global.t) = Global.empty "files" in
   let wfile () = (Global.get files)#current_tpage in
   let file () = (wfile ())#file in
   let output_device = Global.empty "output_device" in

   let add_file f =
      (Global.get files)#append_tpage ~activate:true (file_widget f)
   in
   let m_file_new () = add_file (MidiFile.create 240)
   and m_file_open () =
      let filenames = FileDialog.get_open_filenames (Global.get g_window) in
      let open_file fn = add_file (Import.import_file fn) in
      profile (fun () -> List.iter open_file filenames)
   and m_file_saveas () =
      let filename = FileDialog.get_save_filename (Global.get g_window) in
      profile (fun () -> Export.export_file (file ()) filename)
   and m_refresh_devices () =
      let m = Global.get output_device in
      List.iter (fun i -> m#remove i) m#all_children;
      let devices = MidiIo.enum_output_devices () in
      let have_active = ref false in
      let items =
         let group = ref None in
         let create {MidiIo.id; MidiIo.name} =
            let label = id ^ "    " ^ name in
            let active = (MidiIo.get_output_device () = id) in
            let item = GMenu.radio_menu_item ?group:!group ~label ~active () in
            if active then have_active := true;
            group := Some (Option.default item#group !group);
            let _ = item#connect#activate (fun () ->
               MidiIo.set_output_device id
            ) in
            m#append (item :> GMenu.menu_item);
            item
         in
         List.map create devices
      in
      if not !have_active then
         (List.hd items)#activate ();
   in
   let dynitem s ?modi ?key clb =
      let clb' () =
         set_status "";
         try clb () with
         e -> set_status ("E: " ^ (Printexc.to_string e))
      in
      dynmenuitem s ?modi ?key clb'
   in
   let item label ?modi ?key clb =
      dynitem (S.const (label, true)) ?modi ?key clb
   in
   let accel = GtkData.AccelGroup.create () in
   let _C_S = [`CONTROL; `SHIFT] in
   let file_s, update_file = S.create ~eq:(==) None in
   let undo_s = file_s |> S.map (fun f ->
      try "Undo " ^ (Option.get f)#undo_name, true
      with _ -> "Undo", false
   ) and redo_s = file_s |> S.map (fun f ->
      try "Redo " ^ (Option.get f)#redo_name, true
      with _ -> "Redo", false
   ) in
   window ~g:g_window ~accel ~title:"GtkSugar Test" (
      vbox [
         `fill, menubar ~accel [
            menu "File" [
               item "New"                   ~key:_N m_file_new;
               item "Open..."               ~key:_O m_file_open;
               item "Save"                  ~key:_S (fun () -> ());
               item "Save as..." ~modi:_C_S ~key:_S m_file_saveas;
               item "Quit"                  ~key:_Q GMain.Main.quit;
            ];
            menu "Edit" [
               dynitem undo_s               ~key:_Z (fun () -> (wfile ())#undo);
               dynitem redo_s               ~key:_Y (fun () -> (wfile ())#redo);
            ];
            menu "Settings" [
               menu ~gm:output_device "Output device" [];
               item "Refresh devices" m_refresh_devices;
               item "Test sound" (fun () -> MidiIo.output_note 0 60 1.0);
            ];
         ];
         `expand, tnotebook ~g:files ~callback:update_file ();
         `fill, statusbar ~g:g_statusbar ();
      ]
   ) |> ignore;
   m_refresh_devices ()

let main () =
   MidiIo.init ();
   MidiIo.set_program 0 0;
   create_main_window ();
   run [Global.get g_window];
   MidiIo.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
