
let hello () =
   Midiio.output_note 0 60 1.0;
   flush stdout;;

let delete_event ev =
   print_endline "Delete event occurred";
   flush stdout;
   (*
    * The value returned from this function indicates whether the event should
    * be propagated further by the GTK event handling mechanism. Returning true
    * indicates that the event has been handled, and that it should not
    * propagate further. Returning false continues the normal event handling
    *)
   false;;

let destroy () = GMain.Main.quit ();;

let main () =
   let _ = GtkMain.Main.init () in
   let window = GWindow.window ~border_width:10 () in
   let _ = window#event#connect#delete ~callback:delete_event in
   let _ = window#connect#destroy ~callback:destroy in
   let button = GButton.button ~label:"Hello World" ~packing:window#add () in
   let _ = button#connect#clicked ~callback:hello in
   let print_device d = Printf.printf "Device: %s\n" d; flush stdout in
   Midiio.init ();
   let devices = Midiio.enum_output_devices () in
   let device = List.hd devices in
   List.iter print_device devices;
   Midiio.set_output_device device;
   Midiio.set_program 0 0;
   Printf.printf "Set device: %s\n" device;
   flush stdout;
   window#show ();
   GMain.Main.main ();
   Midiio.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
