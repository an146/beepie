open GMain

let hello () = Midiio.output_note 0 60 1.0;;
let delete_event ev = false;;
let destroy () = Main.quit ();;

let main () =
   let _ = GtkMain.Main.init () in
   Midiio.init ();

   (* Window *)
   let window = GWindow.window ~border_width:0 () in
   let _ = window#event#connect#delete ~callback:delete_event in
   let _ = window#connect#destroy ~callback:destroy in
   let vbox = GPack.vbox ~packing:window#add () in

   let _ = Menu.create vbox#pack in
   let button = GButton.button ~label:"Hello World" ~packing:vbox#add () in
   let _ = button#connect#clicked ~callback:hello in

   Midiio.set_program 0 0;
   window#maximize ();
   window#show ();
   Main.main ();
   Midiio.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
