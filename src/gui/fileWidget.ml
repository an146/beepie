open Batteries
open GtkBase
open GtkSugar
open MidiFile
open MidiNote
open React

let prerender f tracks m =
   let s = m.start and e = m.start + m.len in
   let notes () =
      let f (c, _) = List.mem (F.channel_owner c f) tracks in
      List.enum m.notes |> Enum.filter f
   in
   let parts =
      Enum.fold (fun m (_, n) ->
         let add x m = if s < x && x < e then PSet.add x m else m in
         m |> add n.stime |> add n.etime
      ) (PSet.singleton e) (notes ())
   in
   Enum.map (fun t ->
      t, notes () // (fun (_, n) -> n.stime < t && t <= n.etime)
   ) (PSet.enum parts)

class file_widget initfile =
   let tracks_table = GPack.table () in
   let tab_area = GMisc.drawing_area () in
   let box = vbox [
      `fill,   tracks_table#coerce;
      `fill,   separator `HORIZONTAL;
      `expand, notebook ~show_tabs:false [
         scrolled_window [
            tab_area#coerce;
         ];
      ]
   ] in
   let tracks_table_rows = Stack.create () in
   let hist_s, set_hist = S.create ~eq:(==) ([], []) in
   let file_s = hist_s |> S.map ~eq:(==) (function
      | (f, _) :: _, _ -> f
      | _ -> initfile
   ) in
   let tracks_s, set_tracks = S.create [] in
   let _ =
      let s =
         S.Pair.pair ~eq:(
            fun (f, ts) (f', ts') -> f == f' && ts = ts'
         ) file_s tracks_s
      in
      let up (f, ts) =
         ()
      in
      attach_signal (S.trace up s) box
   in
   object (self)
      inherit pseudo_widget box#coerce

      method file = S.value file_s
      method file_signal = file_s
      method history = S.value hist_s
      method history_signal = hist_s

      method commit desc f =
         (*if Player.file () == Some (self#file) then
            failwith "can't commit while playing";*)
         match self#history with
         (l, r) -> set_hist ((f, desc) :: l, [])

      method commit_map desc fn =
         self#commit desc (fn self#file)

      method undo =
         match self#history with
         | (e :: l, r) -> set_hist (l, e :: r)
         | _ -> failwith "nothing to undo"

      method redo =
         match self#history with
         | (l, e :: r) -> set_hist (e :: l, r)
         | _ -> failwith "nothing to redo"

      initializer
         let up n =
            while Stack.length tracks_table_rows > n do
               Stack.pop tracks_table_rows |> List.iter (fun w -> w#destroy ())
            done;
            let oldrows = Stack.length tracks_table_rows in
            if n > 0 then tracks_table#set_rows n;
            for i = oldrows to n - 1 do
               let attach j (exp, w) =
                  w#misc#set_can_focus false;
                  let expand =
                     match exp with
                     | `fill -> `NONE
                     | `expand -> `X
                  in
                  if j >= tracks_table#columns then
                     tracks_table#set_columns (j + 1);
                  tracks_table#attach ~left:j ~top:i ~expand w;
               in
               let btn = button ~relief:`NONE in
               let sep () = `fill, separator `VERTICAL in

               let track_s =
                  let eq (f, t) (f', t') = f == f' && t = t' in
                  S.map ~eq (fun f -> f, F.track i f) file_s
               in
               let volume_s = S.map (F.volume) track_s in
               let set_volume v =
                  let ft = S.value track_s in
                  if F.volume ft != v then
                     self#commit "Set Volume" (F.set_volume v ft)
               in
               let row = [
                  `fill,   btn (string_of_int (i + 1));
                  sep ();
                  `fill,   btn "M";
                  sep ();
                  `fill,   btn "S";
                  sep ();
                  `expand, btn "Name";
                  sep ();
                  `expand, btn "Instr";
                  sep ();
                  `expand, slider ~signal:(S.map float_of_int volume_s)
                                  ~callback:(int_of_float |- set_volume)
                                  ~move_callback:(Printf.printf "move: %f\n%!")
                                  ~step_incr:1.0 ~page_incr:7.0
                                  `HORIZONTAL (0.0, 127.0);
               ] in
               List.iteri attach row;
               Stack.push (List.map snd row) tracks_table_rows;
            done
         in
         attach_signal (S.map up (S.map F.tracks_count file_s)) tracks_table
   end

let file_widget = new file_widget

(* vim: set ts=3 sw=3 tw=80 : *)
