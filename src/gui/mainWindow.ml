let _ = GtkMain.Main.init ();;

let window = GWindow.window ~border_width:0 ();;
let vbox = GPack.vbox ~packing:window#add ();;
let menu = new Menu.menu vbox#pack;;
let files = GPack.notebook ~packing:vbox#add ();;

let init () =
   let _ = window#connect#destroy ~callback:GMain.Main.quit in
   menu#refresh_devices ();;

(* vim: set ts=3 sw=3 tw=80 : *)
