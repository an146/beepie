let choose_files ?(allow_multiple = false) action =
   let chooser = GWindow.file_chooser_dialog ~action ~parent: MainWindow.window () in
   let accept_stock = if action = `OPEN then `OPEN else `SAVE in
   chooser#add_select_button_stock accept_stock `ACCEPT;
   chooser#add_button_stock `CANCEL `CANCEL;
   chooser#set_select_multiple allow_multiple;
   let result = chooser#run () in
   let ret = chooser#get_filenames in
   chooser#destroy ();
   if result != `ACCEPT then [] else ret;;

let get_open_filenames () = choose_files `OPEN ~allow_multiple: true;;

exception DialogCancelled;;

let get_save_filename () =
   match choose_files `SAVE with
      [] -> raise DialogCancelled
   |  l -> List.hd l;;

let main () =
   Midiio.init ();
   Midiio.set_program 0 0;

   MainWindow.init ();
   let window = MainWindow.window in
   window#maximize ();
   window#show ();

   GMain.Main.main ();
   Midiio.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
