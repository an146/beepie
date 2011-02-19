open Batteries
open FileWidget
open GtkSugar

let test_window () =
   let files = Global.empty "files" in
   let window_content =
      vbox [
         menubar [
            menu "File" [
               menuitem "New" (fun () -> ());
               menuitem "Open" (fun () -> ());
               submenu "Recent" [
                  menuitem "1" (fun () -> ());
                  menuitem "2" (fun () -> ());
               ]
            ];
         ];
         tnotebook ~g:files ~expand:true ();
      ]
   in
   let file = MidiFile.create 240 in
   let wfile = file_widget file in
   (Global.get files)#append_tpage wfile;
   wfile#set_file (MidiFile.add_track wfile#file);
   wfile#set_file (MidiFile.add_track wfile#file);
   wfile#set_file (MidiFile.add_track wfile#file);
   window ~title:"GtkSugar Test" window_content

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
