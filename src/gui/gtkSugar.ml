open Batteries
open React

(** A simple, coercible widget *)
type widget = GObj.widget

(** A basic Gtk+ window type *)
type window = GWindow.window

type widget_entry = window -> widget * bool option

(** Cast everything to a simple widget form *)
let coerce w = w#coerce

(** Convert a widget to a Gtk+ widget *)
let to_gtk_widget w = GObj.as_widget w

(** The base Gtk+ window *)
let window ?callbacks ~title entry =
  let w = GWindow.window ~title () in
  ignore (w#connect#destroy GMain.quit);
  Option.may (List.iter (fun f -> ignore (w#connect#destroy f))) callbacks;
  let (content, _) = entry w in
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
let box f ?expand contents wnd =
  let (box : GPack.box) = f () in
  List.iter (fun e ->
    let (w, exp) = e wnd in
    box#pack ?expand:exp w
  ) contents;
  coerce box, expand

(** Vertical and horizontal boxes for widget packing *)
let vbox = box GPack.vbox
let hbox = box GPack.hbox

(** Drawing area *)
let drawing_area ?expand ?callbacks width height wnd =
  let area = GMisc.drawing_area ~width ~height () in
  connect_callbacks ?callbacks area;
  coerce area, expand

(** Layout *)
let layout ?expand ?callbacks layout_width layout_height wnd =
  let layout = GPack.layout ~layout_width ~layout_height () in
  connect_callbacks ?callbacks layout;
  coerce layout, expand

(** Scrolled window *)
let scrolled_window ?expand width height child wnd =
  let sw = GBin.scrolled_window ~width ~height () in
  sw#add child;
  coerce sw, expand

(** Slider *)
let slider ?expand ?callbacks ?signal ?init ?step orientation (lower, upper) wnd =
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
let combo_box_text ?expand ?callbacks strings wnd =
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

class pseudo_widget (w : widget) =
  object
    method coerce = w
    method get_oid = w#get_oid
  end

let notebook ?g ?expand pages wnd =
  let n = GPack.notebook () in
  let add_page p = p wnd |> fst |> n#append_page |> ignore in
  List.iter add_page pages;
  setg g n;
  coerce n, expand

class ['a] tnotebook =
  let notebook = GPack.notebook () in
  object (self)
    constraint 'a = #pseudo_widget
    inherit pseudo_widget notebook#coerce

    val pages = new GUtil.memo ()
    method notebook = notebook

    method append_tpage ?(activate = false) (p : 'a) =
      let i = notebook#append_page p#coerce in
      if activate then
        notebook#goto_page i;
      pages#add p

    method get_tpage w =
      try pages#find w
      with Not_found -> failwith "page not found"

    method current_tpage =
      notebook#current_page |> notebook#get_nth_page |> self#get_tpage
  end

let tnotebook ?g ?expand wnd =
  let n = new tnotebook in
  setg g n;
  coerce n, expand

let statusbar ?g ?expand _ =
  let sb = GMisc.statusbar () in
  setg g sb;
  coerce sb, expand

let menubar ?expand ?(modi : Gdk.Tags.modifier list = [`CONTROL]) menus wnd =
  let mb = GMenu.menu_bar () in
  let ag = GtkData.AccelGroup.create () in
  List.iter (fun m -> mb#append (m ag modi)) menus;
  wnd#add_accel_group ag;
  coerce mb, expand

type menu_entry = Gtk.accel_group -> Gdk.Tags.modifier list -> GMenu.menu_item

let menu ?g ?gm label ?modi items =
  let item = GMenu.menu_item ~label () in
  let menu = GMenu.menu ~packing:item#set_submenu () in
  setg g item;
  setg gm menu;
  fun ag modi' -> (
    let modi = Option.default modi' modi in
    List.iter (fun item -> menu#append (item ag modi)) items;
    menu#set_accel_group ag;
    item
  )

let menuitem ?g label ?modi ?key callback =
  let item = GMenu.menu_item ~label () in
  let _ = item#connect#activate callback in
  setg g item;
  fun ag modi' -> (
    let modi = Option.default modi' modi in
    Option.may (fun key ->
      item#add_accelerator ~group:ag ~modi ~flags:[`VISIBLE] key
    ) key;
    item
  )

(* vim: set ts=2 sw=2 tw=80 : *)
