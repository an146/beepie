open Batteries
open GtkSugar
open React

class file_widget initfile =
   let tracks_table = GPack.table () in
   let box = vbox [
      `fill,   tracks_table#coerce;
      `fill,   separator `HORIZONTAL;
      `expand, vbox [];
   ] in
   let tracks_table_rows = Stack.create () in
   let file_signal, set_file = S.create ~eq:(==) initfile in
   let tracks_updater =
      let up n =
         while Stack.length tracks_table_rows > n do
            Stack.pop tracks_table_rows |> List.iter (fun w -> w#destroy ())
         done;
         let oldrows = Stack.length tracks_table_rows in
         if n > 0 then tracks_table#set_rows n;
         for i = oldrows to n - 1 do
            let attach j (exp, w) =
               Gobject.Property.set w#as_widget GtkBase.Widget.P.can_focus false;
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
               `expand, slider ~init:0.0 ~step:1.0 `HORIZONTAL (0.0, 127.0);
            ] in
            List.iteri attach row;
            Stack.push (List.map snd row) tracks_table_rows;
         done
      in
      S.map up (S.map MidiFile.tracks_count file_signal)
   in
   object (self)
      inherit pseudo_widget box#coerce

      val tracks_updater = tracks_updater
      method file = S.value file_signal
      method file_signal = file_signal
      method set_file f = set_file f
   end

let file_widget = new file_widget

(* vim: set ts=3 sw=3 tw=80 : *)
