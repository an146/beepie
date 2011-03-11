open Batteries
open Gdk
open GdkEvent
open GtkSugar
open MidiFile
open MidiNote
open React
module C = GnoCanvas
module R = Gdk.Rectangle

let font = Font.load "-*-lucidabright-medium-r-*-*-18-*-*-*-*-*-*-*"

type tw = {
   file_s : file S.t;
   mutable tracks : track_id list;
   mwidth : float DynArray.t;
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

let prerender tw m =
   let _ = TabRender.render_measure (file tw) tw.tracks m in
   let s = m.start and e = m.start + m.len in
   let notes =
      let f = file tw in
      List.filter (fun n ->
         List.mem (F.channel_owner n.channel f |> Option.get) tw.tracks
      ) m.notes
   in
   let parts =
      Enum.fold (fun m n ->
         let add x m = if s < x && x < e then PSet.add x m else m in
         m |> add n.stime |> add n.etime
      ) (PSet.singleton s |> PSet.add e) (List.enum notes)
   in
   let getpairs =
      Enum.map (fun elt -> elt, elt)
      |- Enum.scan (fun (px, _) (x, _) -> (x, px))
      |- Enum.skip 1
   in
   PSet.enum parts
   |> getpairs
   |> Enum.map (fun (t, pt) ->
      let snotes s =
         let first = ref true in
         List.enum notes |> Enum.filter_map (fun n ->
            if n.stime < t && t <= n.etime && n.str = s then
               let f = n.midipitch - n.str in
               let s = string_of_int f in
               let s = if f < 0 then "(" ^ s ^ ")" else s in
               let s = if n.stime < pt then "-" ^ s else s in
               let s = if t < n.etime then s ^ "-" else s in
               let s = if not !first then ", " ^ s else s in
               first := false;
               let w = Font.string_measure font s |> wy in
               Some (n, s, w)
            else
               None
         )
      in
      let get_snotes_len =
         Enum.map (fun (_, _, l) -> l) |- Enum.fold (+.) 0.
      in
      let l =
         List.enum strings |> Enum.map snotes
         |> Enum.map get_snotes_len |> Enum.fold max 0.
      in
      t, l +. 2. *. npadding, List.enum strings /@ snotes
   )

let update_mwidth tw =
   DynArray.clear tw.mwidth;
   F.measures (file tw) |> Vect.enum |> Enum.map (fun m ->
      let w =
         prerender tw m
         |> Enum.map (fun (_, l, _) -> l)
         |> Enum.fold (+.) 0.
      in
      (*Printf.printf "m: %i, %f\n%!" m.start w;*)
      if w > 0. then w else 1.
   ) |> Enum.iter (DynArray.add tw.mwidth);
   redraw tw

let rows tw =
   if DynArray.empty tw.mwidth then
      update_mwidth tw;
   let w = fl tw.tab#width /. unitsize in
   let i = ref 0 in
   Enum.from (fun () ->
      let s = ref 0. in
      let i_start = !i in
      (try
         while !s < w do
            let ns = !s +. DynArray.get tw.mwidth !i in
            if ns > w && !s > 0. then
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

(*
let make_anchor root ~x ~y =
   let grp = GnoCanvas.group ~x ~y root in
   GnoCanvas.rect grp ~props:[
      `X1 (-0.2); `Y1 (-0.2); `X2 0.2; `Y2 0.2;
      `OUTLINE_COLOR "black"; `WIDTH_PIXELS 0
   ] |> ignore;
   grp
*)

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
         Enum.clone e |> Enum.map (DynArray.get tw.mwidth) |> Enum.reduce (+.)
      in
      let xunitsize = (fl tw.tab#width) /. rwidth in
      let rx = ref 0. in
      let ms = F.measures (file tw) in
      let cx x = truncate (x *. xunitsize +. 0.5) in
      let rect ?filled ~x ~y ~w ~h () =
         drawing#rectangle ~x:(cx x)
                           ~y:(cy y)
                           ~width:(cx w)
                           ~height:(cy h)
                           ?filled ()
      in
      Enum.iter (fun m ->
         let x = !rx and w = DynArray.get tw.mwidth m in
         rx := x +. w;
         let poffset = ref 0. in
         drawing#set_foreground `BLACK;
         rect ~x ~y ~w ~h:rh ();
         let asc = Font.ascent font |> wy
         and desc = Font.descent font |> wy in
         prerender tw (Vect.get ms m)
         |> Enum.iter (fun (t, l, ss) ->
               ss |> Enum.iter (fun s ->
                  let soffset = ref 0. in
                  Enum.iter (fun (n, txt, w) ->
                     let h = asc +. desc in
                     let x = x +. !poffset +. !soffset +. npadding in
                     let y =
                        let (i, _) = List.findi (fun _ s -> s = n.str) strings in
                        y +. rh -. fl i -. 0.5 +. h /. 2. -. desc
                     in
                     drawing#set_foreground `BLACK;
                     drawing#string txt ~font:font ~x:(cx x) ~y:(cy y);
                     soffset := !soffset +. w
                     (*
                     drawing#set_foreground (`NAME "red");
                     rect ~x ~y ~w ~h ()
                     *)
                  ) s
               );
               poffset := !poffset +. l;
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
