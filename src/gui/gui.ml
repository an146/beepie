
let hello () =
  print_endline "Hello World";
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

let destroy () = GMain.Main.quit ();;

type io_device_type = InputDevice | OutputDevice;;

external io_init: unit -> unit = "io_init"
external io_fini: unit -> unit = "io_fini"
external io_enum_devices: io_device_type -> string list = "io_enum_devices";;
external io_set_device: io_device_type -> string -> unit = "io_set_device";;
external io_output: string -> unit = "io_output";;
external io_flush_output: unit -> unit = "io_flush_output";;

let io_enum_output_devices () = io_enum_devices OutputDevice;;
let io_set_output_device = io_set_device OutputDevice;;

let main () =
  let _ = GtkMain.Main.init () in
  let window = GWindow.window ~border_width:10 () in
  let _ = window#event#connect#delete ~callback:delete_event in
  let _ = window#connect#destroy ~callback:destroy in
  let button = GButton.button ~label:"Hello World" ~packing:window#add () in
  let _ = button#connect#clicked ~callback:hello in
  let _ = button#connect#clicked ~callback:window#destroy in
  let print_device d = Printf.printf "Device: %s\n" d; flush stdout in
  io_init ();
  List.iter print_device (io_enum_output_devices ());
  window#show ();
  GMain.Main.main ();
  io_fini ();;

let _ = main ();;
