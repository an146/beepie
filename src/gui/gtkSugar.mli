open Batteries

(** Abstract widget and window types *)
type widget_t
type window_t

(** Cast un-wrapped GObj widgets to the appropriate widget form.  This provides
    a simple, if unclean, work-around for widgets which have not been wrapped
    yet. *)
val of_gobj_widget : GObj.widget -> widget_t

(** Cast widget_t widgets to Gtk.widget Gtk.obj values *)
val to_gtk_widget : widget_t -> Gtk.widget Gtk.obj

val coerce : < coerce : 'a; .. > -> 'a

(** Create a GUI window, containing one widget.  When the window is closed it
    will automatically end the GUI loop. *)
val window :
  ?callbacks:(unit -> unit) list -> title:string -> widget_t * bool option -> window_t

(** Show a window *)
val show : window_t -> unit

(** Given a list of windows, show them and run the main Gtk+ loop *)
val run : window_t list -> unit

(** Queue up a widget for update.  Probably most useful in callbacks. *)
val queue_draw : widget_t -> unit

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
  (widget_t * bool option) list -> widget_t * bool option

val hbox :
  ?expand:bool ->
  (widget_t * bool option) list -> widget_t * bool option

(** Drawing area widget which can be used for custom widgets *)
val drawing_area :
  ?expand:bool ->
  ?callbacks:(GMisc.drawing_area -> event_callback_t) list ->
  int -> int -> widget_t * bool option

val layout :
  ?expand:bool ->
  ?callbacks:(GPack.layout -> event_callback_t) list ->
  int -> int -> widget_t * bool option

val scrolled_window :
  ?expand:bool ->
  int -> int -> widget_t -> widget_t * bool option

(** Slider widget for adjusting a value.  If [signal] is provided then the
    value of that signal will follow the slider's value.  *)
val slider :
  ?expand:bool ->
  ?callbacks:(GRange.scale -> unit) list ->
  ?signal:float React.S.t ->
  ?init:float ->
  ?step:float ->
  Gtk.Tags.orientation -> (float * float) -> widget_t * bool option

(** Text-only combo box *)
val combo_box_text :
  ?expand:bool ->
  ?callbacks:(string option -> unit) list ->
  string list -> widget_t * bool option

val notebook :
  ?g:GPack.notebook Global.t ->
  ?expand:bool ->
  (widget_t * bool option) list -> widget_t * bool option

class ['a] tnotebook :
  object
    constraint 'a = <coerce : GObj.widget; ..>
    method append_tpage : 'a -> unit
    method coerce : widget_t
    method current_tpage : 'a
    method get_tpage : widget_t -> 'a
    method notebook : GPack.notebook
  end

val tnotebook :
  ?g:('a tnotebook) Global.t ->
  ?expand:bool ->
  unit -> widget_t * bool option

val menubar :
  ?expand:bool ->
  GMenu.menu_item list -> widget_t * bool option

val menu :
  string -> GToolbox.menu_entry list -> GMenu.menu_item

val submenu :
  string -> GToolbox.menu_entry list -> GToolbox.menu_entry

val menuitem :
  string -> (unit -> unit) -> GToolbox.menu_entry

(* vim: set ts=2 sw=2 tw=80 : *)
