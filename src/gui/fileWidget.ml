open Batteries
open GtkSugar
open React

class file_widget initfile =
   let box = GPack.vbox () in
   let packing = box#pack in
   let tracks_table = GPack.table ~columns:5 ~packing () in
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
            let row = ref [] in
            let j = ref 0 in
            let attach ?expand w =
               let w = w#coerce in
               let expand =
                  Option.map (fun e -> if e then `X else `NONE) expand
               in
               tracks_table#attach ~left:!j ~top:i ?expand w;
               row := w :: !row;
               j := !j + 1
            in
            GButton.button ~relief:`HALF ~label:(string_of_int i) () |> attach;
            GButton.button ~relief:`HALF ~label:"M" () |> attach;
            GButton.button ~relief:`HALF ~label:"S" () |> attach;
            GButton.button ~relief:`HALF ~label:"Name" () |> attach ~expand:true;
            GButton.button ~relief:`HALF ~label:"Instr" () |> attach ~expand:true;
            let s = GRange.scale `HORIZONTAL ~draw_value:false () in
            s#adjustment#set_bounds ~lower:0.0 ~upper:127.0 ~step_incr:1.0 ();
            s |> attach ~expand:true;
            Stack.push !row tracks_table_rows;
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
