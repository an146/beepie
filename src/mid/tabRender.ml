open Batteries
open MidiFile
open MidiNote
open MiscUtils

type elt_value = [
   | `Nothing
   | `Note of note
]

type elt = {
   x_chars : int;
   x_spaces : int;
   y : int;
   track : track_id;
   text : string;
   value : elt_value;
}

let strings = [40; 45; 50; 55; 59; 64]

let render_measure f tracks m =
   let ngroups = List.enum m.notes |> enum_group2 (fun n -> n.stime) in
   let column = Hashtbl.create 20 in
   let x = ref 0 in
   let x_s = ref 0 in
   let stridx = Hashtbl.create 6 in
   List.iteri (fun i s -> Hashtbl.add stridx s i) strings;
   Enum.map (fun notes ->
      Enum.iter (fun n ->
         assert (n.str >= 0);
         let tr = F.channel_owner n.channel f |> Option.get in
         let q =
            try Hashtbl.find column (tr, n.str)
            with _ -> Queue.create () |> tap (Hashtbl.add column (tr, n.str))
         in
         Queue.push n q
      ) notes;
      let x0 = !x in
      let x_s0 = !x_s in
      x_s := !x_s + 1;
      Hashtbl.enum column |> Enum.map (fun ((tr, str), q) ->
         let x' = ref 0 in
         let note_elt n =
            let txt =
               let s = Printf.sprintf "%i" (n.midipitch - n.str) in
               if !x' > 0 then "," ^ s else s
            in
            let elt = {
               x_chars = x0 + !x';
               x_spaces = x_s0;
               y = Hashtbl.find stridx n.str;
               track = tr;
               text = txt;
               value = `Nothing;
            } in
            x' := !x' + String.length txt;
            x := max !x (x0 + !x');
            elt
         in
         Queue.enum q /@ note_elt
      ) |> Enum.flatten |> tap Enum.force
   ) ngroups |> Enum.flatten

(* vim: set ts=3 sw=3 tw=80 : *)
