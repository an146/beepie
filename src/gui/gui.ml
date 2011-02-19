open Batteries
open FileWidget
open GtkSugar

let test_window () =
   let files = Global.empty "files" in
   let window_content =
      vbox [
         (*menu [
            submenu "File" [
               menuitem "New";
               menuitem "Open";
            ];
         ];*)
         tnotebook ~g:files ();
      ]
   in
   let file = MidiFile.create 240 in
   (Global.get files)#append_tpage (file_widget file);
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
