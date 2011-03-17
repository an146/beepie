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

let xunit, yunit =
   let s = C.image_surface_create C.FORMAT_A1 ~width:1 ~height:1 in
   let c = C.create s in
   select_font c Style.font;
   let ext = C.font_extents c in
   ext.C.max_x_advance, ext.C.font_height

let fl x = float_of_int x

let row_height tw = Style.track_height *. (List.length tw.tracks |> fl)

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
   let rx = ref (0., 0.) in
   let cw (c, s) = (c +. s *. ssize) *. xunit in
   let cx (c, s) = Style.left_margin *. xunit +. cw (c, s) in
   let render_measure m =
      let ms = F.measures (file tw) in
      let x = !rx and w = DynArray.get tw.mwidth m in
      rx := x +: w;
      (*
      let rect ~x ~y ~w ~h =
         C.rectangle c ~x:(cx x) ~y:(cy y) ~width:(cw w) ~height:(ch h)
      in
      C.set_source_rgb c ~red:0.0 ~green:0.0 ~blue:0.0;
      rect ~x ~y ~w ~h;
      C.stroke c;
      *)
      render_measure (file tw) tw.tracks (Vect.get ms m)
      |> Enum.iter (fun elt ->
         let x = x +: (0., 1.) +: elt.x in
         let y = y +. h -. fl elt.y -. 0.5 in
         C.set_source_rgb c ~red:0.0 ~green:0.0 ~blue:0.0;
         C.move_to c ~x:(cx x) ~y:(cy y);
         C.show_text c elt.text
      )
   in
   Enum.iter render_measure e

let expose tw r =
   let c = CG.create tw.tab#bin_window in
   select_font c Style.font;
   let _ = (* Fill background *)
      C.set_source_rgb c ~red:1.0 ~green:1.0 ~blue:0.9;
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
