open Batteries
open GtkBase
open GtkSugar
open MidiFile
open MidiNote
open React
open TabWidget
module R = Gdk.Rectangle

class file_widget initfile =
   let hist_s, set_hist = S.create ~eq:(==) ([], []) in
   let file_s = hist_s |> S.map ~eq:(==) (function
      | (f, _) :: _, _ -> f
      | _ -> initfile
   ) in
   let tracks_table = GPack.table () in
   let tab = new tabwidget file_s in
   let box = vbox [
      `fill,   tracks_table#coerce;
      `fill,   separator `HORIZONTAL;
      `expand, notebook ~show_tabs:false [
         tab#coerce
      ]
   ] in
   let tracks_table_rows = Stack.create () in
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
               let btn lbl clb = button ~relief:`NONE ~callbacks:[
                  button_callback (fun _ _ -> clb (); false)
               ] lbl in
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
                  `fill,   btn (string_of_int (i + 1)) (fun () -> 
                     tab#set_tracks [S.value track_s |> snd]
                  );
                  sep ();
                  `fill,   btn "M" (fun () -> ());
                  sep ();
                  `fill,   btn "S" (fun () -> ());
                  sep ();
                  `expand, btn "Name" (fun () -> ());
                  sep ();
                  `expand, btn "Instr" (fun () -> ());
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
