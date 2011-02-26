open Batteries
open GtkBase
open GtkSugar
open React
module File = MidiFile
module Track = MidiTrack

type hist_zipper = {
   l : (File.t * string) list;
   r : (File.t * string) list;
}

let shift_left h =
   match h with
   | {l = e :: l; r} -> {l; r = e :: r}
   | _ -> raise (Invalid_argument "shift_left")

let shift_right h =
   match h with
   | {l; r = e :: r} -> {l = e :: l; r}
   | _ -> raise (Invalid_argument "shift_right")

class file_widget initfile =
   let tracks_table = GPack.table () in
   let box = vbox [
      `fill,   tracks_table#coerce;
      `fill,   separator `HORIZONTAL;
      `expand, vbox [];
   ] in
   let tracks_table_rows = Stack.create () in
   let file_s, set_file = S.create ~eq:(==) initfile in
   let _ =
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

            let track_s = S.map (File.track i) file_s in
            let volume_s =
               S.map (Track.tvalue Ctrl.volume |- float_of_int) track_s
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
               `expand, slider ~signal:volume_s
                               ~step_incr:1.0 ~page_incr:7.0
                               `HORIZONTAL (0.0, 127.0);
            ] in
            List.iteri attach row;
            Stack.push (List.map snd row) tracks_table_rows;
         done
      in
      attach_value (S.map up (S.map File.tracks_count file_s)) tracks_table
   in
   object (self)
      inherit pseudo_widget box#coerce

      (* zipper, actually *)
      val mutable hist = { l = []; r = []; }

      method file = S.value file_s
      method file_signal = file_s

      method set_history h =
         hist <- h;
         let f = try fst (List.hd hist.l) with _ -> initfile in
         set_file f

      method commit f desc =
         self#set_history {l = (f, desc) :: hist.l; r = []}

      method undo_name =
         try List.hd hist.l |> snd
         with _ -> failwith "no undo items"

      method redo_name =
         try List.hd hist.r |> snd
         with _ -> failwith "no redo items"

      method undo =
         try self#set_history (shift_left hist)
         with _ -> failwith "nothing to undo"

      method redo =
         try self#set_history (shift_right hist)
         with _ -> failwith "nothing to redo"
   end

let file_widget = new file_widget

(* vim: set ts=3 sw=3 tw=80 : *)
