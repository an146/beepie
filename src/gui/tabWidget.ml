open Batteries
open GdkEvent
open GtkSugar
open MidiFile
open MidiNote
open React
module C = GnoCanvas
module R = Gdk.Rectangle

type tw = {
   file_s : file S.t;
   mutable tracks : track_id list;
   mwidth : float DynArray.t;
   cnv : C.canvas;
   sw : GBin.scrolled_window;
   mutable rendered: (int, C.group) PMap.t;
}

let unitsize = 15.0

let fl x = float_of_int x

let track_height = 15.0

let row_height tw = track_height *. (List.length tw.tracks |> fl)

let strings = [40; 45; 50; 55; 59; 64]

let file tw = S.value tw.file_s

let prerender tw m =
   let s = m.start and e = m.start + m.len in
   let notes =
      let f = file tw in
      List.filter (fun (c, _) ->
         List.mem (F.channel_owner c f |> Option.get) tw.tracks
      ) m.notes
   in
   let parts =
      Enum.fold (fun m (_, n) ->
         let add x m = if s < x && x < e then PSet.add x m else m in
         m |> add n.stime |> add n.etime
      ) (PSet.singleton e) (List.enum notes)
   in
   Enum.map (fun t ->
      let snotes s =
         List.enum notes |> Enum.filter (fun (_, n) ->
            n.stime < t && t <= n.etime && n.str = s
         )
      in
      t, List.enum strings /@ snotes
   ) (PSet.enum parts)

let update_mwidth tw =
   DynArray.clear tw.mwidth;
   F.measures (file tw) |> Vect.enum |> Enum.map (fun m ->
      let w =
         prerender tw m
         |> Enum.map snd |> Enum.flatten
         |> Enum.map Enum.count |> Enum.sum |> fl
      in
      2. +. if w > 0. then w else 1.
   ) |> Enum.iter (DynArray.add tw.mwidth)

let rows tw =
   if DynArray.empty tw.mwidth then
      update_mwidth tw;
   let w = fl tw.cnv#width /. unitsize in
   let i = ref 0 in
   Enum.from (fun () ->
      let s = ref 0. in
      let i_start = !i in
      (try
         while !s < w do
            let ns = !s +. DynArray.get tw.mwidth !i in
            if ns > w then
               failwith "it's enough";
            s := ns;
            incr i
         done;
      with _ -> ());
      if !s <= 0. then
         raise Enum.No_more_elements;
      assert (!i > i_start);
      (i_start -- (!i - 1))
   )

let expose tw r =
   let rh = row_height tw in
   let r1, r2 =
      let row y = truncate (fl y /. unitsize /. rh) in
      row (R.y r), row (R.y r + R.height r)
   in
   Enum.combine (rows tw, (0 -- max_int))
   |> Enum.skip r1
   |> Enum.take (r2 - r1 + 1)
   |> Enum.iter (fun (e, i) ->
      let y = fl i *. rh in
      let rwidth =
         Enum.clone e |> Enum.map (DynArray.get tw.mwidth) |> Enum.reduce (+.)
      in
      let stretch = (fl tw.cnv#width /. unitsize) /. rwidth in
      let rx = ref 0. in
      let ms = F.measures (file tw) in
      Enum.iter (fun m ->
         let w = DynArray.get tw.mwidth m in
         let x = !rx in
         let x' = x +. w in
         if not (PMap.mem m tw.rendered) then (
            Printf.printf "m: %i\n%!" m;
            let g = C.group ~x:(x *. stretch) ~y tw.cnv#root in
            ignore x';
            let _ =
               C.rect ~x1:0. ~y1:0. ~x2:(w *. stretch) ~y2:rh ~fill_color:"red" g
               ~props:[`OUTLINE_COLOR "black"]
            in
            tw.rendered <- PMap.add m g tw.rendered
         );
         prerender tw (Vect.get ms m) |> ignore;
         rx := x'
      ) e
   );
   (*Printf.printf "e: %i %i\n%!" (R.y r) (R.width r);*)
   false

let resize tw {Gtk.width = rw} =
   PMap.iter (fun _ g -> g#destroy ()) tw.rendered;
   tw.rendered <- PMap.empty;
   let w = fl rw /. unitsize in
   let h = (rows tw |> Enum.count |> fl) *. row_height tw in
   tw.cnv#set_scroll_region ~x1:0. ~y1:0. ~x2:w ~y2:h;
   Printf.printf "w: %i\n%!" rw

let vscroll tw a =
   ()
   (*Printf.printf "y: %f\n%!" a#value*)

let create file_s =
   let tw =
      let sw = GBin.scrolled_window ~hpolicy:`NEVER () in
      let cnv = C.canvas ~packing:sw#add ~width:600 ~height:450 () in
      (*cnv#set_center_scroll_region false;*)
      cnv#set_pixels_per_unit unitsize;
      {
         file_s;
         tracks = [1002] |> Obj.magic;
         mwidth = DynArray.create ();
         cnv;
         sw;
         rendered = PMap.empty;
      }
   in
   (*GnoCanvas.rect tw.cnv#root
      ~props:[ `X1 0.; `Y1 0.; `X2 600.; `Y2 450. ;
      `OUTLINE_COLOR "black" ; `WIDTH_UNITS 4. ] |> ignore;
      *)
   expose_callback (fun _ ev -> expose tw (Expose.area ev)) tw.cnv;
   resize_callback (fun _ r -> resize tw r) tw.cnv;
   vadj_changed_callback (fun _ a -> vscroll tw a) tw.cnv;
   tw

class tabwidget file_s =
   let tw = create file_s in
   object (self)
      inherit pseudo_widget tw.sw
   end

(* vim: set ts=3 sw=3 tw=80 : *)
