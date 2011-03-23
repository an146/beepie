open Batteries
open GToolbox
open GtkBase
open GtkSugar
open MidiFile
open MidiNote
open React
open TabWidget
module R = Gdk.Rectangle

type 'a zipper = 'a list * 'a list

type fw = {
   hist_s : (file * string) zipper S.t;
   set_hist : (file * string) zipper -> unit;
   file_s : file S.t;
   box : widget;
   tab : tabwidget;
   trtable : GPack.table;
   trtable_rows : widget list Stack.t;
}

let hist fw = S.value fw.hist_s

let commit fw desc f =
   match hist fw with
   (l, r) -> fw.set_hist ((f, desc) :: l, [])

let set_tracks_count fw n =
   while Stack.length fw.trtable_rows > n do
      Stack.pop fw.trtable_rows |> List.iter (fun w -> w#destroy ())
   done;
   let oldrows = Stack.length fw.trtable_rows in
   if n > 0 then fw.trtable#set_rows n;
   for i = oldrows to n - 1 do
      let attach j (exp, w) =
         w#misc#set_can_focus false;
         let expand =
            match exp with
            | `fill -> `NONE
            | `expand -> `X
         in
         if j >= fw.trtable#columns then
            fw.trtable#set_columns (j + 1);
         fw.trtable#attach ~left:j ~top:i ~expand w;
      in
      let btn_s lbl_s clb = button ~relief:`NONE ~callbacks:[
         button_callback (fun _ _ -> clb (); false)
      ] lbl_s in
      let btn lbl = btn_s (S.const lbl) in
      let sep () = `fill, separator `VERTICAL in

      let track_s =
         let eq (f, t) (f', t') = f == f' && t = t' in
         S.map ~eq (fun f -> f, F.track i f) fw.file_s
      in
      let volume_s = S.map (F.volume) track_s in
      let set_volume v =
         let ft = S.value track_s in
         if F.volume ft != v then
           commit fw "Set Volume" (F.set_volume v ft)
      in
      let row = [
         `fill,   btn (string_of_int (i + 1)) (fun () -> 
            fw.tab#set_tracks [S.value track_s |> snd]
         );
         sep ();
         `fill,   btn "M" (fun () -> ());
         sep ();
         `fill,   btn "S" (fun () -> ());
         sep ();
         `expand, btn_s (S.map (uncurry F.track_name) track_s) (fun () ->
            Option.may (fun name ->
               let f = track_s |> S.value |> uncurry (F.set_track_name name) in
               commit fw "Set Track Name" f
            ) (input_string ~title:"Set track name" "Enter track name:")
         );
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
      Stack.push (List.map snd row) fw.trtable_rows;
   done

let create initfile =
   let hist_s, set_hist = S.create ~eq:(==) ([], []) in
   let file_s = hist_s |> S.map ~eq:(==) (function
      | (f, _) :: _, _ -> f
      | _ -> initfile
   ) in
   let trtable = GPack.table () in
   let tab = new tabwidget file_s in
   let box = vbox [
      `fill,   trtable#coerce;
      `fill,   separator `HORIZONTAL;
      `expand, notebook ~show_tabs:false [
         tab#coerce
      ]
   ] in
   let trtable_rows = Stack.create () in
   let fw = {hist_s; set_hist; file_s; box; tab; trtable; trtable_rows} in
   attach_signal (
      S.map (set_tracks_count fw) (S.map F.tracks_count file_s)
   ) trtable;
   fw

class file_widget initfile =
   let fw = create initfile in
   object (self)
      inherit pseudo_widget fw.box#coerce

      method file = S.value fw.file_s
      method file_signal = fw.file_s
      method history = S.value fw.hist_s
      method history_signal = fw.hist_s

      method commit desc f = commit fw desc f

      method commit_map desc fn =
         self#commit desc (fn self#file)

      method undo =
         match self#history with
         | (e :: l, r) -> fw.set_hist (l, e :: r)
         | _ -> failwith "nothing to undo"

      method redo =
         match self#history with
         | (l, e :: r) -> fw.set_hist (e :: l, r)
         | _ -> failwith "nothing to redo"
   end

let file_widget = new file_widget

(* vim: set ts=3 sw=3 tw=80 : *)
