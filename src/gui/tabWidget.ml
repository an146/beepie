open Batteries
open GdkEvent
open GtkSugar
open MidiFile
open MidiNote
open React
module C = GnoCanvas
module R = Gdk.Rectangle

let strings = [40; 45; 50; 55; 59; 64]

type tw = {
   file_s : file S.t;
   mutable tracks : track_id list;
   mwidth : int DynArray.t;
   cnv : C.canvas;
   sw : GBin.scrolled_window;
   mutable rendered: (int, C.group) PMap.t;
}

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
         |> Enum.map Enum.count |> Enum.sum
      in
      if w > 0 then w else 1
   ) |> Enum.iter (DynArray.add tw.mwidth)

let rows tw =
   if DynArray.empty tw.mwidth then
      update_mwidth tw;
   let w = tw.cnv#width in
   let i = ref 0 in
   Printf.printf "measures: %i\n" (DynArray.length tw.mwidth);
   Enum.from (fun () ->
      let s = ref 0 in
      let i_start = !i in
      (try
         while !s < w do
            let min_part_width = 20 in
            let ns = !s + (DynArray.get tw.mwidth !i) * min_part_width in
            if ns > w then
               failwith "it's enough";
            s := ns;
            incr i
         done;
      with _ -> ());
      if !s <= 0 then
         raise Enum.No_more_elements;
      assert (!i > i_start);
      (i_start -- (!i - 1))
   )

let fl x = float_of_int x

let expose tw r =
   let track_height = 100 in
   let row_height = track_height * List.length tw.tracks in
   let r1, r2 =
      let row y = y / row_height in
      row (R.y r), row (R.y r + R.height r)
   in
   rows tw
   |> Enum.skip r1
   |> Enum.take (r2 - r1 + 1)
   |> Enum.iteri (fun i e ->
      let y = (i - 1) * row_height |> fl in
      let parts =
         Enum.clone e |> Enum.map (DynArray.get tw.mwidth) |> Enum.sum
      in
      let partw = (fl tw.cnv#width) /. (fl parts) in
      let rx = ref 0 in
      let ms = F.measures (file tw) in
      ignore partw;
      Enum.iter (fun m ->
         Printf.printf "m: %i\n%!" m;
         let w = DynArray.get tw.mwidth m in
         let x = !rx in
         let x' = x + w in
         if not (PMap.mem m tw.rendered) then (
            let x = fl x and x' = fl x' and rh = fl row_height in
            let g = C.group ~x:x ~y tw.cnv#root in
            let _ = C.rect ~x1:x ~y1:y ~x2:x' ~y2:(y +. rh) ~fill_color:"red" g in
            tw.rendered <- PMap.add m g tw.rendered
         );
         prerender tw (Vect.get ms m) |> ignore;
         rx := x'
      ) e
   );
   Printf.printf "e: %i %i\n%!" (R.y r) (R.width r);
   false

let resize tw =
   update_mwidth tw;
   let sr = tw.cnv#get_scroll_region in
   tw.cnv#set_scroll_region ~x1:sr.(0) ~y1:sr.(1) ~x2:sr.(2) ~y2:10000.0;
   Printf.printf "w: %i\n%!" tw.cnv#width

let vscroll tw a =
   Printf.printf "y: %f\n%!" a#value

let create file_s =
   let tw =
      let sw = GBin.scrolled_window ~hpolicy:`NEVER () in
      let cnv = C.canvas ~packing:sw#add ~width:600 ~height:450 () in
      cnv#set_center_scroll_region false;
      {
         file_s;
         tracks = [1003] |> Obj.magic;
         mwidth = DynArray.create ();
         cnv;
         sw;
         rendered = PMap.empty;
      }
   in
   GnoCanvas.rect tw.cnv#root
      ~props:[ `X1 0.; `Y1 0.; `X2 600.; `Y2 450. ;
      `OUTLINE_COLOR "black" ; `WIDTH_UNITS 4. ] |> ignore;
   expose_callback (fun _ ev -> expose tw (Expose.area ev)) tw.cnv;
   resize_callback (fun _ _ -> resize tw) tw.cnv;
   vadj_changed_callback (fun _ a -> vscroll tw a) tw.cnv;
   tw

class tabwidget file_s =
   let tw = create file_s in
   object (self)
      inherit pseudo_widget tw.sw
   end

(* vim: set ts=3 sw=3 tw=80 : *)
