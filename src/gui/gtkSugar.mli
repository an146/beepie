open Batteries

(** Abstract widget and window types *)
type widget = GObj.widget
type window = GWindow.window

(** Cast widget widgets to Gtk.widget Gtk.obj values *)
val to_gtk_widget : widget -> Gtk.widget Gtk.obj

class pseudo_widget :
  widget ->
  object
    method coerce : widget
    method get_oid : int
  end

val coerce : #pseudo_widget -> widget

val attach_signal : 'a React.S.t -> #GObj.widget -> unit

(** Create a GUI window, containing one widget.  When the window is closed it
    will automatically end the GUI loop. *)
val window :
  ?g: window Global.t ->
  ?accel:Gtk.accel_group ->
  ?callbacks:(unit -> unit) list -> title:string -> widget -> window

(** Show a window *)
val show : window -> unit

(** Given a list of windows, show them and run the main Gtk+ loop *)
val run : window list -> unit

(** Queue up a widget for update.  Probably most useful in callbacks. *)
val queue_draw : widget -> unit

(** Event callbacks *)
type event_callback_t
val any_callback :
  ('a -> Gdk.Tags.event_type Gdk.event -> bool) -> 'a -> event_callback_t
val button_callback :
  ('a -> GdkEvent.Button.t -> bool) -> 'a -> event_callback_t
val scroll_callback :
  ('a -> GdkEvent.Scroll.t -> bool) -> 'a -> event_callback_t
val expose_callback :
  ('a -> GdkEvent.Expose.t -> bool) -> 'a -> event_callback_t
val configure_callback :
  ('a -> GdkEvent.Configure.t -> bool) -> 'a -> event_callback_t

type boxing_type = [`expand | `fill]

(** Box widgets, for housing other widgets *)
val vbox :
  (boxing_type * widget) list -> widget

val hbox :
  (boxing_type * widget) list -> widget

val button :
  ?relief:Gtk.Tags.relief_style ->
  string ->
  widget

(** Drawing area widget which can be used for custom widgets *)
val drawing_area :
  ?callbacks:(GMisc.drawing_area -> event_callback_t) list ->
  int -> int -> widget

val layout :
  ?callbacks:(GPack.layout -> event_callback_t) list ->
  int -> int -> widget

val scrolled_window :
  int -> int -> widget -> widget

(** Slider widget for adjusting a value.  If [signal] is provided then the
    value of that signal will follow the slider's value.  *)
val slider :
  ?callback:(float -> unit) ->
  ?move_callback:(float -> unit) ->
  ?signal:float React.S.t ->
  ?init:float ->
  ?step_incr:float ->
  ?page_incr:float ->
  Gtk.Tags.orientation -> (float * float) -> widget

(** Text-only combo box *)
val combo_box_text :
  ?callbacks:(string option -> unit) list ->
  string list -> widget

val notebook :
  ?g:GPack.notebook Global.t ->
  widget list -> widget

class ['a] tnotebook :
  object
    inherit pseudo_widget

    constraint 'a = #pseudo_widget
    method append_tpage : ?activate:bool -> 'a -> unit
    method coerce : widget
    method current_tpage : 'a
    method get_tpage : int -> 'a
    method notebook : GPack.notebook
    method tpage_signal : 'a option React.S.t
  end

val tnotebook :
  ?g:('a tnotebook) Global.t ->
  ?callback:('a option -> unit) ->
  unit ->
  widget

val statusbar :
  ?g:(GMisc.statusbar Global.t) ->
  unit ->
  widget

val separator :
  Gtk.Tags.orientation ->
  widget

type menu_entry = Gtk.accel_group -> Gdk.Tags.modifier list -> GMenu.menu_item

val menubar :
  accel:Gtk.accel_group ->
  ?modi:Gdk.Tags.modifier list ->
  menu_entry list ->
  widget

val menu :
  ?g:GMenu.menu_item Global.t ->
  ?gm:GMenu.menu Global.t ->
  string ->
  ?modi:Gdk.Tags.modifier list ->
  menu_entry list ->
  menu_entry

val menuitem :
  ?g:GMenu.menu_item Global.t ->
  string ->
  ?modi:Gdk.Tags.modifier list ->
  ?key:Gdk.keysym ->
  (unit -> unit) ->
  menu_entry

val dynmenuitem :
  ?g:GMenu.menu_item Global.t ->
  (string * bool) React.S.t ->
  ?modi:Gdk.Tags.modifier list ->
  ?key:Gdk.keysym ->
  (unit -> unit) ->
  menu_entry

(* vim: set ts=2 sw=2 tw=80 : *)
