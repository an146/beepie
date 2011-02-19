open Batteries
open React

(** A simple, coercible widget *)
type widget_t = GObj.widget

(** A basic Gtk+ window type *)
type window_t = GWindow.window

(** Convert a GObj.widget to a widget_t *)
external of_gobj_widget : GObj.widget -> widget_t = "%identity"

(** Cast everything to a simple widget form *)
let coerce w = w#coerce

(** Convert a widget_t to a Gtk+ widget *)
let to_gtk_widget w = GObj.as_widget w

(** The base Gtk+ window *)
let window ?callbacks ~title (content, _) =
  let w = GWindow.window ~title () in
  ignore (w#connect#destroy GMain.quit);
  Option.may (List.iter (fun f -> ignore (w#connect#destroy f))) callbacks;
  w#add content;
  w

(** Show a window *)
let show w = w#show ()

(** Given a set of windows, run the GUI! *)
let run windows =
  List.iter show windows;
  GMain.main ()

(** Queue up a widget for update.  Probably most useful in callbacks. *)
let queue_draw widget = GtkBase.Widget.queue_draw (to_gtk_widget widget)

(** Events support by this module.  Should be abstract. *)
type event_callback_t =
  | Any of (Gdk.Tags.event_type Gdk.event -> bool)
  | Button_press of (GdkEvent.Button.t -> bool)
  | Scroll of (GdkEvent.Scroll.t -> bool)
  | Expose of (GdkEvent.Expose.t -> bool)
  | Configure of (GdkEvent.Configure.t -> bool)

(** User-visible functions to create callbacks for each event type *)
let any_callback f x = Any (f x)
let button_callback f x = Button_press (f x)
let scroll_callback f x = Scroll (f x)
let expose_callback f x = Expose (f x)
let configure_callback f x = Configure (f x)

(** HIDDEN - For use in connecting callbacks to events *)
let connect_callback widget event_callback =
  let connect = widget#event#connect in
  (* Connect the callback to the event, ignoring the generated signal id *)
  let f ?e_add e_connection e_callback =
    ignore (e_connection ~callback:e_callback);
    Option.may (fun x -> widget#event#add [x]) e_add;
  in
  match event_callback with
  | Any a_f -> f connect#any a_f
  | Button_press b_f -> f ~e_add:`BUTTON_PRESS connect#button_press b_f
  | Scroll s_f -> f ~e_add:`SCROLL connect#scroll s_f
  | Expose e_f -> f ~e_add:`EXPOSURE connect#expose e_f
  | Configure c_f -> f connect#configure c_f
let connect_callbacks ?callbacks widget =
  Option.may (
    fun callbacks ->
      let callbacks = List.map (fun f -> f widget) callbacks in
      List.iter (fun callback -> connect_callback widget callback) callbacks;
  ) callbacks

(*
(** Event box, for capturing input events *)
let event_box ~callbacks contents =
  let e_box = GBin.event_box () in
  (* Add the given widgets to the event box *)
  List.iter (fun widget -> e_box#add widget#coerce) contents;
  (* Connect event callbacks to the event box *)
  List.iter (fun callback -> connect_callback e_box callback) callbacks;
  e_box
*)

(* Support for box building *)
let box f ?expand contents =
  let (box : GPack.box) = f () in
  List.iter (fun (w, exp) -> box#pack ?expand:exp w) contents;
  coerce box, expand

(** Vertical and horizontal boxes for widget packing *)
let vbox = box GPack.vbox
let hbox = box GPack.hbox

(** Drawing area *)
let drawing_area ?expand ?callbacks width height =
  let area = GMisc.drawing_area ~width ~height () in
  connect_callbacks ?callbacks area;
  coerce area, expand

(** Layout *)
let layout ?expand ?callbacks layout_width layout_height =
  let layout = GPack.layout ~layout_width ~layout_height () in
  connect_callbacks ?callbacks layout;
  coerce layout, expand

(** Scrolled window *)
let scrolled_window ?expand width height child =
  let sw = GBin.scrolled_window ~width ~height () in
  sw#add child;
  coerce sw, expand

(** Slider *)
let slider ?expand ?callbacks ?signal ?init ?step orientation (lower, upper) =
  let s = GRange.scale `HORIZONTAL ~draw_value:false () in
  s#adjustment#set_bounds ~lower ~upper ?step_incr:step ();
  (* Create a signal which tracks changes in the slider *)
  let signal, send =
    (* Default to the lowest value if not initializing value is provided. *)
    let init = Option.default lower init in
    let signal, send = S.create init in
    let signal = S.trace s#adjustment#set_value signal in
    signal, send
  in
  (* Make sure the slider and the signal stay synchronized *)
  let _ = s#connect#value_changed (fun () -> send s#adjustment#value) in
  Option.may (
    fun callbacks ->
      List.iter (
        fun callback ->
          ignore (s#connect#value_changed (fun () -> callback s))
      ) callbacks;
  ) callbacks;
  coerce s, expand

(** Text combo-box *)
let combo_box_text ?expand ?callbacks strings =
  let (combo, _) as combo_full = GEdit.combo_box_text ~strings () in
  Option.may (
    fun callbacks ->
      List.iter (
        fun callback ->
          ignore (
            combo#connect#changed (
              fun () ->
                callback (GEdit.text_combo_get_active combo_full)
            )
          )
      ) callbacks;
  ) callbacks;
  coerce combo, expand

let setg g w = Option.may (fun g -> Global.set g w) g

let notebook ?g ?expand pages =
  let n = GPack.notebook () in
  let add_page (p, _) = n#append_page p |> ignore in
  List.iter add_page pages;
  setg g n;
  coerce n, expand

class ['a] tnotebook =
  object (self)
    val notebook = GPack.notebook ()
    val mutable pages = []

    method coerce = notebook#coerce
    method notebook = notebook

    method append_tpage (p : 'a) =
      let w = p#coerce in
      let _ = notebook#append_page w in
      pages <- (w#get_oid, p) :: pages;

    method get_tpage w =
      try List.assoc w#get_oid pages
      with Not_found -> failwith "page not found"

    method current_tpage =
      notebook#current_page |> notebook#get_nth_page |> self#get_tpage
  end

let tnotebook ?g ?expand () =
  let n = new tnotebook in
  setg g n;
  coerce n, expand

let menubar ?expand menus =
  let m = GMenu.menu_bar () in
  List.iter m#append menus;
  coerce m, expand

let menu label entries =
  let item = GMenu.menu_item ~label () in
  let menu = GMenu.menu ~packing:item#set_submenu () in
  GToolbox.build_menu menu entries;
  item

let submenu name entries =
  `M (name, entries)

let menuitem name callback =
  `I (name, callback)

(* vim: set ts=2 sw=2 tw=80 : *)
