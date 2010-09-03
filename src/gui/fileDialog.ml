let choose_files ?(allow_multiple = false) action parent =
   let chooser = GWindow.file_chooser_dialog ~action ~parent () in
   let accept_stock = if action = `OPEN then `OPEN else `SAVE in
   chooser#add_select_button_stock accept_stock `ACCEPT;
   chooser#add_button_stock `CANCEL `CANCEL;
   chooser#set_select_multiple allow_multiple;
   let result = chooser#run () in
   let ret = chooser#get_filenames in
   chooser#destroy ();
   if result != `ACCEPT then [] else ret;;

let get_open_filenames parent = choose_files `OPEN ~allow_multiple: true parent;;

exception DialogCancelled;;

let get_save_filename parent =
   match choose_files `SAVE parent with
      [] -> raise DialogCancelled
   |  l -> List.hd l;;
