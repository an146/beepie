open Batteries
open FileWidget
open GdkKeysyms
open GtkSugar

let g_window = Global.empty "window"

let create_main_window () =
   let files = Global.empty "files" in
   let add_file f = (Global.get files)#append_tpage (file_widget f) in
   let m_file_new () = add_file (MidiFile.create 240) in
   let m_file_open () =
      let filenames = FileDialog.get_open_filenames (Global.get g_window) in
      let open_file fn = add_file (Import.import_file fn) in
      List.iter open_file filenames
   in
   window ~title:"GtkSugar Test" (
      vbox [
         menubar [
            menu "File" [
               menuitem "New"        ~key:_N m_file_new;
               menuitem "Open..."    ~key:_O m_file_open;
               menuitem "Save"       ~key:_S (fun () -> ());
               menuitem "Save as..." ~modi:[`CONTROL; `SHIFT] ~key:_S (fun () -> ());
               menuitem "Quit"       ~key:_Q GMain.Main.quit;
            ];
         ];
         tnotebook ~g:files ~expand:true;
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
