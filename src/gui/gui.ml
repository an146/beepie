open Batteries
open FileWidget
open GdkKeysyms
open GtkSugar

let test_window () =
   let files = Global.empty "files" in
   let window_content =
      vbox [
         menubar [
            menu "File" [
               menuitem "New"        ~key:_N (fun () -> ());
               menuitem "Open..."    ~key:_O (fun () -> ());
               menuitem "Save"       ~key:_S (fun () -> ());
               menuitem "Save as..." ~modi:[`CONTROL; `SHIFT] ~key:_S (fun () -> ());
               menuitem "Quit"       ~key:_Q GMain.Main.quit;
            ];
         ];
         tnotebook ~g:files ~expand:true;
      ]
   in
   let file = MidiFile.create 240 in
   let wfile = file_widget file in
   let wnd = window ~title:"GtkSugar Test" window_content in
   (Global.get files)#append_tpage wfile;
   wfile#set_file (MidiFile.add_track wfile#file);
   wfile#set_file (MidiFile.add_track wfile#file);
   wfile#set_file (MidiFile.add_track wfile#file);
   wnd

let main () =
   MidiIo.init ();
   MidiIo.set_program 0 0;

   MainWindow.init ();
   MainWindow.refresh_devices ();
   let window = MainWindow.window in
   window#maximize ();
   window#show ();

   run [test_window ()];
   MidiIo.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
