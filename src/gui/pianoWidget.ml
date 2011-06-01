open Batteries
open CairoUtils
open Gdk
open GdkEvent
open GtkSugar
open MidiFile
open MidiNote
open React
module C = Cairo
module CG = Cairo_lablgtk
module R = Gdk.Rectangle
module Style = PianoWidgetStyle

type pw = {
   file_s : file S.t;
   track_s : track_id S.t;
   area : GPack.layout;
   sw : GBin.scrolled_window;
}

let file pw = S.value pw.file_s
let track pw = S.value pw.track_s

let expose tw r =
   let c = CG.create tw.area#bin_window in
   let () = (* Fill background *)
      set_source_color c Style.background;
      CG.rectangle c r;
      C.fill c;
   in
   false

let create file_s track_s =
   let pw =
      let sw = GBin.scrolled_window () in
      let area = GPack.layout ~packing:sw#add () in
      let rec pw = {
         file_s;
         track_s;
         sw;
         area;
      } in
      pw
   in
   expose_callback (fun _ ev -> expose pw (Expose.area ev)) pw.area;
   pw

class pianowidget file_s track_s =
   let pw = create file_s track_s in
   object (self)
      inherit pseudo_widget pw.sw
   end

(* vim: set ts=3 sw=3 tw=80 : *)
