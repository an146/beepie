open Batteries
open Gdk
open GdkEvent
open GtkSugar
open MidiFile
open MidiNote
open React
open TabRender
module C = GnoCanvas
module R = Gdk.Rectangle

let font = Font.load "-*-lucidabright-medium-r-*-*-18-*-*-*-*-*-*-*"

type tw = {
   file_s : file S.t;
   mutable tracks : track_id list;
   mwidth : tabx DynArray.t;
   tab : GPack.layout;
   sw : GBin.scrolled_window;
}

let unitsize = 30.0

let fl x = float_of_int x

let cy y = truncate (y *. unitsize +. 0.5)
let wy y = (fl y) /. unitsize

let track_height = 8.0

let row_height tw = track_height *. (List.length tw.tracks |> fl)

let npadding = 0.2

let strings = [40; 45; 50; 55; 59; 64]

let file tw = S.value tw.file_s

let redraw tw =
   queue_draw tw.tab#coerce

let update_mwidth tw =
   let f = file tw in
   DynArray.clear tw.mwidth;
   F.measures f |> Vect.enum |> Enum.map (fun m ->
      let w =
         render_measure f tw.tracks m
         |> Enum.map (fun elt -> elt.x)
         |> Enum.fold tabx_max (0., 0.)
      in
      (* measure delimiting space *)
      w +: (0., 2.)
   ) |> Enum.iter (DynArray.add tw.mwidth);
   redraw tw

let calc_space_size tw (w_chars, w_spaces) =
   let charsize = unitsize in
   let w = fl tw.tab#width /. charsize in
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
               Float.abs (1.0 -. ssize)
               +. (if ssize < 0.5 then 10.0 else 0.0)
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

let expose tw r =
   let rh = row_height tw in
   let r1, r2 =
      let row y = truncate (fl y /. unitsize /. rh) in
      row (R.y r), row (R.y r + R.height r)
   in
   let drawing = new GDraw.drawable tw.tab#bin_window in
   (* Background *)
   drawing#set_foreground (`RGB (0xffff, 0xffff, 0xd000));
   drawing#rectangle ~filled:true ~x:(R.x r) ~y:(R.y r)
                     ~width:(R.width r) ~height:(R.height r) ();
   Enum.combine (rows tw, (0 -- max_int))
   |> Enum.skip r1
   |> Enum.take (r2 - r1 + 1)
   |> Enum.iter (fun (e, i) ->
      let y = fl i *. rh in
      let rwidth =
         Enum.clone e |> Enum.map (DynArray.get tw.mwidth) |> Enum.reduce (+:)
      in
      let ssize = calc_space_size tw rwidth in
      let rx = ref (0., 0.) in
      let ms = F.measures (file tw) in
      Printf.printf "ssize: %f\n%!" ssize;
      let cx (c, s) = (c +. s *. ssize) *. unitsize +. 0.5 |> truncate in
      let rect ?filled ~x ~y ~w ~h () =
         drawing#rectangle ~x:(cx x)
                           ~y:(cy y)
                           ~width:(cx w)
                           ~height:(cy h)
                           ?filled ()
      in
      Enum.iter (fun m ->
         let x = !rx and w = DynArray.get tw.mwidth m in
         rx := x +: w;
         (*
         drawing#set_foreground (`NAME "red");
         rect ~filled:true ~x ~y ~w ~h:rh ();
         *)
         drawing#set_foreground `BLACK;
         rect ~x ~y ~w ~h:rh ();
         let asc = Font.ascent font |> wy
         and desc = Font.descent font |> wy in
         render_measure (file tw) tw.tracks (Vect.get ms m)
         |> Enum.iter (fun elt ->
            let h = asc +. desc in
            let x = x +: (0., 1.) +: elt.x in
            let y =
               y +. rh -. fl elt.y -. 0.5 +. h /. 2. -. desc
            in
            drawing#set_foreground `BLACK;
            drawing#string elt.text ~font:font ~x:(cx x) ~y:(cy y);
            (*
            drawing#set_foreground (`NAME "red");
            rect ~x ~y ~w ~h ()
            *)
         )
      ) e
   );
   (*Printf.printf "e: %i %i\n%!" (R.y r) (R.width r);*)
   false

let calc_height tw = (rows tw |> Enum.hard_count |> fl) *. row_height tw

let readjust_height tw =
   tw.tab#set_height (calc_height tw *. unitsize |> truncate)

let resize tw {Gtk.width = rw} =
   tw.tab#set_width rw;
   readjust_height tw;
   redraw tw
   (*
   let w = fl rw /. unitsize in
   let h = calc_height tw in
   tw.tab#set_width (truncate w);
   tw.tab#set_height (truncate h)
   *)

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
