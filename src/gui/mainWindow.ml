let _ = GtkMain.Main.init ();;

let window = GWindow.window ~border_width: 0 ();;
let vbox = GPack.vbox ~packing: window#add ();;
let packing = vbox#pack ~from: `END;;
let statusbar = GMisc.statusbar ~packing ();;
let status_ctx = statusbar#new_context "Status";;
let files =
   let packing = packing ~expand: true in
   GPack.notebook ~tab_pos:`TOP ~packing ();;

let set_status s =
   status_ctx#pop ();
   ignore (status_ctx#push s);;

let add_file (f : MidiFile.file) =
   let text = "Append Frame" in
   let label = GMisc.label ~text:"Page" () in
   let border_width = 10 in
   let frame =
      let ign f x = ignore (f x) in
      let packing = ign (files#append_page ~tab_label:label#coerce) in
      GBin.frame ~label:text ~border_width ~packing ()
   in
   let _ = GMisc.label ~text ~packing:frame#add () in
   ignore f;;

module Menu = Menu.Make(struct
   let window = window
   let add_file = add_file
   let set_status = set_status
end)

let menu = new Menu.menu packing;;
let refresh_devices = menu#refresh_devices;;

let init () =
   let _ = window#connect#destroy ~callback: GMain.Main.quit in
   ();;

(* vim: set ts=3 sw=3 tw=80 : *)
