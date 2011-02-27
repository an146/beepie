open Batteries
open GtkBase
open GtkSugar
open React
module File = MidiFile
module Track = MidiTrack

class file_widget initfile =
   let tracks_table = GPack.table () in
   let box = vbox [
      `fill,   tracks_table#coerce;
      `fill,   separator `HORIZONTAL;
      `expand, vbox [];
   ] in
   let tracks_table_rows = Stack.create () in
   let hist_s, set_hist = S.create ~eq:(==) ([], []) in
   let file_s = hist_s |> S.map ~eq:(==) (function
      | (f, _) :: _, _ -> f
      | _ -> initfile
   ) in
   object (self)
      inherit pseudo_widget box#coerce

      method file = S.value file_s
      method file_signal = file_s
      method history = S.value hist_s
      method history_signal = hist_s

      method commit desc f =
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

               let track_s = S.map ~eq:(==) (File.track i) file_s in
               let volume_s = S.map (Track.tvalue Ctrl.volume) track_s in
               (*let map_track desc fn =
                  self#commit (File.map_track i fn self#file) desc
               in*)
               let set_volume v =
                  if Track.volume (S.value track_s) != v then
                     self#commit_map "Set Volume" (File.set_volume i v)
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
                                  ~step_incr:1.0 ~page_incr:7.0
                                  ~update_policy:`DISCONTINUOUS
                                  `HORIZONTAL (0.0, 127.0);
               ] in
               List.iteri attach row;
               Stack.push (List.map snd row) tracks_table_rows;
            done
         in
         attach_signal (S.map up (S.map File.tracks_count file_s)) tracks_table
   end

let file_widget = new file_widget

(* vim: set ts=3 sw=3 tw=80 : *)
