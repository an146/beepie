let _ = GtkMain.Main.init ();;

let window = GWindow.window ~border_width: 0 ();;
let vbox = GPack.vbox ~packing: window#add ();;
let files = GPack.notebook ~tab_pos:`TOP ~packing: (vbox#pack ~expand: true ~from: `END) ();;

let add_file (f : Midifile.file) =
   let text = "Append Frame" in
   let label = GMisc.label ~text:"Page" () in
   let border_width = 10 in
   let ign f x = ignore (f x) in
   let frame = GBin.frame ~label:text ~border_width
      ~packing:(ign (files#append_page ~tab_label:label#coerce)) () in
   let _ = GMisc.label ~text ~packing:frame#add () in
   ignore f;;

module Menu = Menu.Make(struct
   let window = window
   let add_file = add_file
end)

let menu = new Menu.menu (vbox#pack ~from: `END);;

let init () =
   let _ = window#connect#destroy ~callback: GMain.Main.quit in
   ();;

let refresh_devices = menu#refresh_devices;;

(* vim: set ts=3 sw=3 tw=80 : *)
