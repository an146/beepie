open GMain
open GdkKeysyms

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

let destroy () = Main.quit ();;

let refresh_devices menu =
   List.iter (fun c -> c#destroy ()) menu#all_children;

   let factory = new GMenu.factory menu in
   let devices = Midiio.enum_output_devices () in
   let have_active = ref false in
   let create_item device group =
      let id = device.Midiio.id in
      let name = device.Midiio.name in
      let callback = fun set -> if set then Midiio.set_output_device id in
      let label = id ^ "    " ^ name in
      let active = (Midiio.get_output_device () = id) in
      if active then have_active := true;
      factory#add_radio_item label ~active ~callback ~group
   in
   let first = create_item (List.hd devices) None in
   let create_item device = create_item device first#group in
   let _ = List.map create_item (List.tl devices) in
   if not !have_active then begin
      first#set_active true;
      Midiio.set_output_device (List.hd devices).Midiio.id
   end;;

let create_menu packing =
   let menubar = GMenu.menu_bar ~packing () in
   let factory = new GMenu.factory menubar in
   let accel_group = factory#accel_group in

   (* top-level *)
   let m_file = factory#add_submenu "File" in
   let m_settings = factory#add_submenu "Settings" in

   (* File *)
   let factory = new GMenu.factory m_file ~accel_group in
   let _ = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

   (* Settings *)
   let factory = new GMenu.factory m_settings ~accel_group in
   let outdevice = factory#add_submenu "Output device" in
   let _ =
      let clb = fun () -> refresh_devices outdevice in
      clb ();
      factory#add_item "Refresh devices" ~callback: clb
   in

   menubar;;

let main () =
   let _ = GtkMain.Main.init () in
   Midiio.init ();

   (* Window *)
   let window = GWindow.window ~border_width:0 () in
   let _ = window#event#connect#delete ~callback:delete_event in
   let _ = window#connect#destroy ~callback:destroy in
   let vbox = GPack.vbox ~packing:window#add () in

   let _ = create_menu vbox#pack in
   let button = GButton.button ~label:"Hello World" ~packing:vbox#add () in
   let _ = button#connect#clicked ~callback:hello in

   Midiio.set_program 0 0;
   window#maximize ();
   window#show ();
   Main.main ();
   Midiio.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
