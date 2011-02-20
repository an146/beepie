open Batteries
open FileWidget
open GdkKeysyms
open GtkSugar

let g_window = Global.empty "window"
and g_statusbar = Global.empty "statusbar"

let set_status =
   let ctx () = (Global.get g_statusbar)#new_context ~name:"Status" in
   fun s -> (
      let ctx = ctx () in
      ctx#pop ();
      ignore (ctx#push s)
   )

let create_main_window () =
   let files = Global.empty "files" in
   let output_device = Global.empty "output_device" in

   let add_file f = (Global.get files)#append_tpage (file_widget f) in
   let m_file_new () = add_file (MidiFile.create 240)
   and m_file_open () =
      let filenames = FileDialog.get_open_filenames (Global.get g_window) in
      let open_file fn = add_file (Import.import_file fn) in
      List.iter open_file filenames
   and m_file_saveas () =
      let file = (Global.get files)#current_tpage#file in
      let filename = FileDialog.get_save_filename (Global.get g_window) in
      Export.export_file file filename
   and m_refresh_devices () =
      let m = Global.get output_device in
      List.iter (fun i -> m#remove i) m#all_children
   in
   let item label ?modi ?key clb =
      let clb' () =
         set_status "";
         try clb () with
         | Failure desc ->
               set_status ("Error: " ^ desc)
         | e ->
               let ename = Printexc.to_string e in
               set_status ("Unhandled exception occurred: " ^ ename)
      in
      menuitem label ?modi ?key clb';
   in
   window ~title:"GtkSugar Test" (
      vbox [
         menubar [
            menu "File" [
               item "New"              ~key:_N m_file_new;
               item "Open..."          ~key:_O m_file_open;
               item "Save"             ~key:_S (fun () -> ());
               item "Save as..."
                  ~modi:[`CONTROL; `SHIFT] ~key:_S m_file_saveas;
               item "Quit"             ~key:_Q GMain.Main.quit;
            ];
            menu "Settings" [
               menu ~gm:output_device "Output device" [];
               item "Refresh devices" m_refresh_devices;
            ];
         ];
         tnotebook ~g:files ~expand:true;
         statusbar ~g:g_statusbar;
      ]
   ) |> Global.set g_window

let main () =
   MidiIo.init ();
   MidiIo.set_program 0 0;

   MainWindow.init ();
   MainWindow.refresh_devices ();
   let window = MainWindow.window in
   window#maximize ();
   window#show ();
   create_main_window ();

   run [Global.get g_window];
   MidiIo.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
