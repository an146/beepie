open Batteries
open Gdk
open GdkEvent
open GtkSugar
open MidiFile
open MidiNote
open React
open TabRender
open TabWidgetStyle
module C = Cairo
module CG = Cairo_lablgtk
module R = Gdk.Rectangle

type tw = {
   file_s : file S.t;
   mutable tracks : track_id list;
   mwidth : tabx DynArray.t;
   tab : GPack.layout;
   sw : GBin.scrolled_window;
}

let xunit, yunit, ascent, descent =
   let s = C.image_surface_create C.FORMAT_A1 ~width:1 ~height:1 in
   let c = C.create s in
   select_font c Style.font;
   let ext = C.font_extents c in
   ext.C.max_x_advance, ext.C.font_height, ext.C.ascent, ext.C.descent

let fl x = float_of_int x

let row_height tw = Style.track_height *. (List.length tw.tracks |> fl)

let cw ssize (c, s) = (c +. s *. ssize) *. xunit
let cx ssize (c, s) = Style.left_margin *. xunit +. cw ssize (c, s)
let ch y = y *. yunit
let cy y = Style.top_margin *. yunit +. (ch y)

let strings = [40; 45; 50; 55; 59; 64]

let file tw = S.value tw.file_s

let redraw tw =
   queue_draw tw.tab#coerce

let update_mwidth tw =
   let f = file tw in
   DynArray.clear tw.mwidth;
   F.measures f |> Vect.enum |> Enum.map (fun m ->
      let w =
         let get_endpoint elt =
            let len = (String.length elt.text |> fl, 0.) in
            elt.x +: len
         in
         render_measure f tw.tracks m
         |> Enum.map get_endpoint
         |> Enum.fold tabx_max (0., 0.)
      in
      (* empty measure contains 1 empty row *)
      let w = if w > (0., 0.) then w else w +: (1., 0.) in
      (* measure delimiting space *)
      w +: (0., 2.)
   ) |> Enum.iter (DynArray.add tw.mwidth);
   redraw tw

let calc_space_size tw (w_chars, w_spaces) =
   let w = (fl tw.tab#width) /. xunit in
   let w = w -. Style.left_margin -. Style.right_margin in
   (w -. w_chars) /. w_spaces

let rows tw =
   if DynArray.empty tw.mwidth then
      update_mwidth tw;
   let mwidth i = DynArray.get tw.mwidth i in
   let i = ref 0 in
   Enum.from (fun () ->
      let q = Queue.create () in
      let push () =
         Queue.push !i q;
         incr i;
      in
      (try
         let s = ref (mwidth !i) in
         push ();
         while true do
            let s' = !s +: mwidth !i in
            let penalty s =
               let ssize = calc_space_size tw s in
               Float.abs (1.3 -. ssize)
               +. (if ssize < 0.9 then 10.0 else 0.0)
            in
            if penalty s' > penalty !s then
               failwith "it's enough";
            s := s';
            push ()
         done
      with _ -> ());
      if Queue.is_empty q then
         raise Enum.No_more_elements
      else
         Queue.enum q
   )

let render_row tw c (e, i) =
   let h = row_height tw in
   let y = fl i *. h in
   let w =
      Enum.clone e |> Enum.map (DynArray.get tw.mwidth) |> Enum.reduce (+:)
   in
   let ssize = calc_space_size tw w in
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
         move_to (0., 0.) y;
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
         |> render_measure (file tw) tw.tracks
         |> Enum.iter (fun elt ->
            let x = x +: (0., 1.) +: elt.x in
            let y = y +. h -. fl elt.y -. descent /. yunit in
            set_source_color c Style.background;
            rect ~x ~y ~w:(String.length elt.text |> fl, 0.) ~h:(~-. 1.0);
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
      x +: DynArray.get tw.mwidth m;
   in
   Enum.fold render_measure (0., 0.) e
   |> measurebar

let expose tw r =
   let c = CG.create tw.tab#bin_window in
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

let calc_height tw = (rows tw |> Enum.hard_count |> fl) *. row_height tw

let readjust_height tw =
   let h = Style.top_margin +. calc_height tw +. Style.bottom_margin in
   ch h |> truncate |> tw.tab#set_height

let resize tw {Gtk.width = rw} =
   tw.tab#set_width rw;
   readjust_height tw;
   redraw tw

let vscroll tw a =
   ()

let create file_s =
   let tw =
      let sw = GBin.scrolled_window ~hpolicy:`NEVER () in
      let tab = GPack.layout ~packing:sw#add () in
      {
         file_s;
         tracks = [1002] |> Obj.magic;
         mwidth = DynArray.create ();
         sw;
         tab;
      }
   in
   expose_callback (fun _ ev -> expose tw (Expose.area ev)) tw.tab;
   resize_callback (fun _ r -> resize tw r) tw.tab;
   vadj_changed_callback (fun _ a -> vscroll tw a) tw.tab;
   tw

class tabwidget file_s =
   let tw = create file_s in
   object (self)
      inherit pseudo_widget tw.sw

      method set_tracks ts =
         tw.tracks <- ts;
         update_mwidth tw;
         readjust_height tw
   end

(* vim: set ts=3 sw=3 tw=80 : *)
