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
   let factory = new GMenu.factory menu in
   let devices = Midiio.enum_output_devices () in
   let group_ref = ref None in
   let add_item device =
      let callback = fun set -> if set then Midiio.set_output_device device in
      let active = (Midiio.get_output_device () = device) in
      let add_item group = factory#add_radio_item device ~active ~callback ~group in
      let item = add_item !group_ref in
      if !group_ref = None then
         group_ref := item#group
   in
   List.iter (fun c -> c#destroy ()) menu#all_children;
   List.iter add_item devices;;

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

   let print_device d = Printf.printf "Device: %s\n" d; flush stdout in
   let devices = Midiio.enum_output_devices () in
   let device = List.hd devices in
   List.iter print_device devices;
   Midiio.set_output_device device;
   Midiio.set_program 0 0;
   Printf.printf "Set device: %s\n" device;
   flush stdout;
   window#maximize ();
   window#show ();
   Main.main ();
   Midiio.fini ();;

let _ = main ();;

(* vim: set ts=3 sw=3 tw=80 : *)
