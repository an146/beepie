open GMain

let hello () = Midiio.output_note 0 60 1.0;;
let delete_event ev = false;;
let destroy () = Main.quit ();;

let choose_files ?(allow_multiple = false) action =
   let chooser = GWindow.file_chooser_dialog ~action () in
   let accept_stock = if action = `OPEN then `OPEN else `SAVE in
   chooser#add_select_button_stock accept_stock `ACCEPT;
   chooser#add_select_button_stock `CANCEL `CANCEL;
   chooser#set_select_multiple allow_multiple;
   let result = chooser#run () in
   let ret = chooser#get_filenames in
   chooser#destroy ();
   if result != `ACCEPT then [] else ret;;

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

   let _ = choose_files `OPEN in

   Midiio.set_program 0 0;
   window#maximize ();
   window#show ();
   Main.main ();
   Midiio.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
