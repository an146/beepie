open Batteries
open GtkSugar
open React

class file_widget initfile =
   let box = GPack.vbox () in
   let packing = box#pack in
   let tracks_table = GPack.table ~packing () in
   let tracks_table_rows = Stack.create () in
   let file_signal, set_file = S.create initfile in
   let tracks_updater =
      let up n =
         while Stack.length tracks_table_rows > n do
            Stack.pop tracks_table_rows |> List.iter (fun w -> w#destroy)
         done;
         let oldrows = tracks_table#rows in
         tracks_table#set_rows n;
         for i = oldrows to n - 1 do
            let ctrls = [
               GButton.button ~label:(string_of_int i) () |> coerce;
               GButton.button ~label:"M" () |> coerce;
               GButton.button ~label:"S" () |> coerce;
            ] in
            List.iteri (fun j c -> tracks_table#attach ~left:j ~top:i c) ctrls
         done
      in
      S.map up (S.map MidiFile.tracks_count file_signal)
   in
   object (self)
      val tracks_updater = tracks_updater
      method coerce = box#coerce
      method file = S.value file_signal
      method file_signal = file_signal
      method set_file f = set_file f
   end

let file_widget = new file_widget

(* vim: set ts=3 sw=3 tw=80 : *)
