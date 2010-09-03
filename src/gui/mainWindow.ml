let _ = GtkMain.Main.init ();;

let add_file (f : Midifile.file) = ignore f;;
let window = GWindow.window ~border_width: 0 ();;

module Menu = Menu.Make(struct
   let window = window
   let add_file = add_file
end)

let vbox = GPack.vbox ~packing: window#add ();;
let menu = new Menu.menu vbox#pack;;
let files = GPack.notebook ~tab_pos:`TOP ~packing: vbox#add ();;

let ign f x = ignore (f x);;

let init () =
   let _ = window#connect#destroy ~callback: GMain.Main.quit in
   for i = 1 to 5 do
      let text = "Append Frame " ^ string_of_int i in
      let label = GMisc.label ~text:("Page " ^ string_of_int i) () in
      let border_width = 10 in
      let frame = GBin.frame ~label:text ~border_width
         ~packing:(ign (files#append_page ~tab_label:label#coerce)) () in
      let _ = GMisc.label ~text ~packing:frame#add () in
      ()
   done;;

let refresh_devices = menu#refresh_devices;;

(* vim: set ts=3 sw=3 tw=80 : *)
