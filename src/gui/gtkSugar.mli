open Batteries

(** Abstract widget and window types *)
type widget = GObj.widget
type window = GWindow.window

type widget_entry = window -> widget * bool option

(** Cast widget widgets to Gtk.widget Gtk.obj values *)
val to_gtk_widget : widget -> Gtk.widget Gtk.obj

val coerce : < coerce : 'a; .. > -> 'a

(** Create a GUI window, containing one widget.  When the window is closed it
    will automatically end the GUI loop. *)
val window :
  ?callbacks:(unit -> unit) list -> title:string -> widget_entry -> window

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

(** Box widgets, for housing other widgets *)
val vbox :
  ?expand:bool ->
  widget_entry list -> widget_entry

val hbox :
  ?expand:bool ->
  widget_entry list -> widget_entry

(** Drawing area widget which can be used for custom widgets *)
val drawing_area :
  ?expand:bool ->
  ?callbacks:(GMisc.drawing_area -> event_callback_t) list ->
  int -> int -> widget_entry

val layout :
  ?expand:bool ->
  ?callbacks:(GPack.layout -> event_callback_t) list ->
  int -> int -> widget_entry

val scrolled_window :
  ?expand:bool ->
  int -> int -> widget -> widget_entry

(** Slider widget for adjusting a value.  If [signal] is provided then the
    value of that signal will follow the slider's value.  *)
val slider :
  ?expand:bool ->
  ?callbacks:(GRange.scale -> unit) list ->
  ?signal:float React.S.t ->
  ?init:float ->
  ?step:float ->
  Gtk.Tags.orientation -> (float * float) -> widget_entry

(** Text-only combo box *)
val combo_box_text :
  ?expand:bool ->
  ?callbacks:(string option -> unit) list ->
  string list -> widget_entry

class pseudo_widget :
  widget ->
  object
    method coerce : widget
    method get_oid : int
  end

val notebook :
  ?g:GPack.notebook Global.t ->
  ?expand:bool ->
  widget_entry list -> widget_entry

class ['a] tnotebook :
  object
    inherit pseudo_widget

    constraint 'a = #pseudo_widget
    method append_tpage : 'a -> unit
    method coerce : widget
    method current_tpage : 'a
    method get_tpage : widget -> 'a
    method notebook : GPack.notebook
  end

val tnotebook :
  ?g:('a tnotebook) Global.t ->
  ?expand:bool ->
  widget_entry

val statusbar :
  ?g:(GMisc.statusbar Global.t) ->
  ?expand:bool ->
  widget_entry

type menu_entry = Gtk.accel_group -> Gdk.Tags.modifier list -> GMenu.menu_item

val menubar :
  ?expand:bool ->
  ?modi:Gdk.Tags.modifier list ->
  menu_entry list ->
  widget_entry

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

(* vim: set ts=2 sw=2 tw=80 : *)
