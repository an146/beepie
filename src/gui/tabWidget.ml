open Batteries
open CairoUtils
open Gdk
open GdkEvent
open GtkSugar
open MidiFile
open MidiNote
open React
open TabRender
open TabWidgetStyle
open TabX.Infix
module C = Cairo
module CG = Cairo_lablgtk
module R = Gdk.Rectangle

type tw = {
   file_s : file S.t;
   tracks_s : track_id list S.t;
   layout : TabLayout.t;
   area : GPack.layout;
   sw : GBin.scrolled_window;
}

let file tw = S.value tw.file_s
let tracks tw = S.value tw.tracks_s

let xunit, yunit, ascent, descent =
   let ext = get_font_extents Style.font in
   ext.C.max_x_advance, ext.C.font_height, ext.C.ascent, ext.C.descent

let fl x = float_of_int x

let row_height tw = Style.track_height *. (tracks tw |> List.length |> fl)

let cw ssize tx = TabX.size tx ssize *. xunit
let cx ssize tx = Style.left_margin *. xunit +. cw ssize tx
let ch y = y *. yunit
let cy y = Style.top_margin *. yunit +. (ch y)

let strings = [40; 45; 50; 55; 59; 64]

let width tw = (fl tw.area#width) /. xunit

let rows tw =
   TabLayout.enum_rows tw.layout (width tw)

let redraw tw =
   queue_draw tw.area#coerce

let calc_height tw =
   let n = rows tw |> Enum.hard_count in
   fl n *. row_height tw

let readjust_height tw =
   let h = Style.top_margin +. calc_height tw +. Style.bottom_margin in
   ch h |> truncate |> tw.area#set_height

let refresh_layout ?f tw =
   let f = Option.default (file tw) f in
   TabLayout.refresh tw.layout f (tracks tw);
   readjust_height tw;
   redraw tw

let space_size tw w =
   let cw = width tw -. Style.left_margin -. Style.right_margin in
   TabX.space_size w cw

let render_row tw c (e, i) =
   let h = row_height tw in
   let y = fl i *. h in
   let w = TabLayout.row_width tw.layout e in
   let ssize = space_size tw w in
   let cx = cx ssize and cw = cw ssize in
   let move_to x y = C.move_to c ~x:(cx x) ~y:(cy y) in
   let line_to x y = C.line_to c ~x:(cx x) ~y:(cy y) in
   let rect ~x ~y ~w ~h =
      C.rectangle c ~x:(cx x) ~y:(cy y) ~width:(cw w) ~height:(ch h)
   in
   let _ = rect in
   let _ = (* strings *)
      set_source_color c Style.string_color;
      for i = 0 to List.length strings - 1 do
         let y = y +. h -. 0.5 -. fl i in
         move_to TabX.zero y;
         line_to w y;
         C.stroke c
      done
   in
   let measurebar x =
      set_source_color c Style.measurebar_color;
      move_to x (y +. h -. 0.5);
      line_to x (y +. h -. 0.5 -. fl (List.length strings - 1));
      C.stroke c
   in
   let render_measure x m =
      measurebar x;
      let _ = (* measure number *)
         select_font c Style.measure_font;
         set_source_color c Style.measure_color;
         C.move_to c ~x:(cx (x +: Style.measure_x)) ~y:(cy (y +. Style.measure_y));
         C.show_text c (string_of_int (m + 1));
      and _ = (* notes *)
         select_font c Style.font;
         Vect.get (F.measures (file tw)) m
         |> render_measure (file tw) (tracks tw)
         |> Enum.iter (fun elt ->
            let x = x +: TabX.space +: elt.x in
            let y = y +. h -. fl elt.y -. descent /. yunit in
            set_source_color c Style.background;
            rect ~x ~y ~w:(TabX.text elt.text) ~h:(~-. 1.0);
            C.fill c;
            set_source_color c Style.foreground;
            C.move_to c ~x:(cx x) ~y:(cy y);
            C.show_text c elt.text
         )
         (*
      and _ = (* measure bounding box *)
         set_source_color c Style.foreground;
         rect ~x ~y ~w ~h;
         C.stroke c;
         *)
      in
      x +: TabLayout.measure_width tw.layout m;
   in
   let endx = Enum.fold render_measure TabX.zero e in
   measurebar endx

let expose tw r =
   let c = CG.create tw.area#bin_window in
   let _ = (* Fill background *)
      set_source_color c Style.background;
      CG.rectangle c r;
      C.fill c;
   in
   let r1, r2 =
      let row y =
         let y = fl y /. yunit -. Style.top_margin in
         y /. (row_height tw) |> truncate
      in
      row (R.y r), row (R.y r + R.height r)
   in
   let _ =
      Enum.combine (rows tw, (0 -- max_int))
      |> Enum.skip r1
      |> Enum.take (r2 - r1 + 1)
      |> Enum.iter (render_row tw c)
   in
   false

let resize tw {Gtk.width = rw} =
   tw.area#set_width rw;
   refresh_layout tw

let vscroll tw a =
   ()

let create file_s tracks_s =
   let tw =
      let sw = GBin.scrolled_window ~hpolicy:`NEVER () in
      let area = GPack.layout ~packing:sw#add () in
      let rec tw = {
         file_s;
         tracks_s;
         layout = TabLayout.create ();
         sw;
         area;
      } in
      tw
   in
   expose_callback (fun _ ev -> expose tw (Expose.area ev)) tw.area;
   resize_callback (fun _ r -> resize tw r) tw.area;
   vadj_changed_callback (fun _ a -> vscroll tw a) tw.area;
   tw

class tabwidget file_s tracks_s =
   let file_s = S.trace (fun f -> Applicature.update_file f strings) file_s in
   let tw = create file_s tracks_s in
   let track_changes s fn = attach_signal (S.trace fn s) tw.sw in
   object (self)
      inherit pseudo_widget tw.sw

      initializer
         track_changes file_s (fun f ->
            refresh_layout ~f tw
         );
         track_changes tracks_s (fun _ ->
            refresh_layout tw
         )
   end

(* vim: set ts=3 sw=3 tw=80 : *)
