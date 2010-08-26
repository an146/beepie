
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

let main () =
  let window = GWindow.window ~border_width:10 () in
  ignore (window#event#connect#delete ~callback:delete_event);
  ignore (window#connect#destroy ~callback:destroy);
  let button = GButton.button ~label:"Hello World" ~packing:window#add () in
  ignore (button#connect#clicked ~callback:hello);
  ignore (button#connect#clicked ~callback:window#destroy);
  window#show ();
  GMain.Main.main ();;

let _ = main ();;
