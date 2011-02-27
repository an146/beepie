open Batteries
open GtkBase
open React

(** A simple, coercible widget *)
type widget = GObj.widget

(** A basic Gtk+ window type *)
type window = GWindow.window

class pseudo_widget (w : widget) =
  object
    method coerce = w
    method get_oid = w#get_oid
  end

(** Cast everything to a simple widget form *)
let coerce (w : #pseudo_widget) = w#coerce

(** Convert a widget to a Gtk+ widget *)
let to_gtk_widget w = GObj.as_widget w

let setg g w = Option.may (fun g -> Global.set g w) g

(** Attach value to widget, keeping it from garbage collection *)
let attach_signal v (w : #GObj.widget) =
  w#misc#connect#destroy ~callback:(fun () -> ignore v) |> ignore

(** The base Gtk+ window *)
let window ?g ?accel ?callbacks ~title entry =
  let w = GWindow.window ~title () in
  ignore (w#connect#destroy GMain.quit);
  setg g w;
  Option.may (List.iter (fun f -> ignore (w#connect#destroy f))) callbacks;
  w#add entry;
  Option.may w#add_accel_group accel;
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

type boxing_type = [`expand | `fill]

(* Support for box building *)
let box f contents =
  let (box : GPack.box) = f () in
  List.iter (fun (exp, e) ->
    let expand =
      match exp with
      | `expand -> true
      | `fill -> false
    in
    box#pack ~expand e
  ) contents;
  coerce box

(** Vertical and horizontal boxes for widget packing *)
let vbox = box GPack.vbox
let hbox = box GPack.hbox

let button ?relief label =
  let btn = GButton.button ?relief ~label () in
  btn#coerce

(** Drawing area *)
let drawing_area ?callbacks width height =
  let area = GMisc.drawing_area ~width ~height () in
  connect_callbacks ?callbacks area;
  coerce area

(** Layout *)
let layout ?callbacks layout_width layout_height =
  let layout = GPack.layout ~layout_width ~layout_height () in
  connect_callbacks ?callbacks layout;
  coerce layout

(** Scrolled window *)
let scrolled_window width height child =
  let sw = GBin.scrolled_window ~width ~height () in
  sw#add child;
  coerce sw

(** Slider *)
let slider ?callback ?signal ?init ?step_incr ?page_incr orientation (lower, upper) =
  let sl = GRange.scale `HORIZONTAL ~draw_value:false () in
  sl#adjustment#set_bounds ~lower ~upper ?step_incr ?page_incr ();
  Option.may sl#adjustment#set_value init;
  Option.may (fun s ->
    let s = S.trace sl#adjustment#set_value s in
    attach_signal s sl
  ) signal;
  Option.may (fun clb ->
    ignore (sl#connect#value_changed (fun () -> clb sl#adjustment#value))
  ) callback;
  coerce sl

(** Text combo-box *)
let combo_box_text ?callbacks strings =
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
  coerce combo

let notebook ?g pages =
  let n = GPack.notebook () in
  let add_page p = p |> n#append_page |> ignore in
  List.iter add_page pages;
  setg g n;
  coerce n

class ['a] tnotebook =
  let ntb = GPack.notebook () in
  let page_s, update_page = S.create ~eq:(==) (None : widget option) in
  object (self)
    constraint 'a = #pseudo_widget
    inherit pseudo_widget ntb#coerce

    val pages = new GUtil.memo ()

    method notebook = ntb

    method append_tpage ?(activate = false) (p : 'a) =
      pages#add p;
      let i = ntb#append_page p#coerce in
      if activate then
        ntb#goto_page i;

    method get_tpage i =
      try pages#find (ntb#get_nth_page i)
      with _ -> failwith "page not found"

    method current_tpage = ntb#current_page |> self#get_tpage

    method tpage_signal = S.map (Option.map pages#find) page_s

    initializer
      let callback i =
        let page =
          if i >= 0 then Some (self#get_tpage i)
          else None
        in
        Option.map coerce page |> update_page
      in
      ntb#connect#switch_page ~callback |> ignore
  end

let tnotebook ?g ?callback () =
  let n = new tnotebook in
  Option.may (fun c ->
    attach_signal (S.map c n#tpage_signal) (coerce n)
  ) callback;
  setg g n;
  coerce n

let statusbar ?g () =
  let sb = GMisc.statusbar () in
  setg g sb;
  coerce sb

let separator o =
  let sep = GMisc.separator o () in
  coerce sep

let menubar ~accel ?(modi : Gdk.Tags.modifier list = [`CONTROL]) menus =
  let mb = GMenu.menu_bar () in
  List.iter (fun m -> mb#append (m accel modi)) menus;
  coerce mb

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

let dynmenuitem ?g s ?modi ?key callback =
  let action =
    let id = ref 0 in
    let name = "action" ^ (string_of_int !id) in
    id := !id + 1;
    GAction.action ~name ()
  in
  let item = GMenu.menu_item () in
  action#connect_proxy item#coerce;
  attach_signal (S.trace (fun (l, e) ->
    action#set_label l;
    action#set_sensitive e
  ) s) item;
  let _ = item#connect#activate callback in
  setg g item;
  fun ag modi' -> (
    let modi = Option.default modi' modi in
    Option.may (fun key ->
      item#add_accelerator ~group:ag ~modi ~flags:[`VISIBLE] key
    ) key;
    item
  )

let menuitem ?g label ?modi ?key callback =
  dynmenuitem ?g (S.const (label, true)) ?modi ?key callback

(* vim: set ts=2 sw=2 tw=80 : *)
