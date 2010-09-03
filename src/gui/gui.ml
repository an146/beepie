let main () =
   Midiio.init ();
   Midiio.set_program 0 0;

   MainWindow.init ();
   MainWindow.refresh_devices ();
   let window = MainWindow.window in
   window#maximize ();
   window#show ();

   GMain.Main.main ();
   Midiio.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
