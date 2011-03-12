open Batteries
open MidiFile
open MidiNote
open MiscUtils

type elt_value = [
   | `Nothing
   | `Note of note
]

type tabx = float * float (* chars * spaces *)

let tabx_op op (a_c, a_s) (b_c, b_s) =
   (op a_c b_c, op a_s b_s)

let (+:) a b = tabx_op (+.) a b
let tabx_max a b = tabx_op max a b

type elt = {
   x : tabx;
   y : int;
   track : track_id;
   text : string;
   value : elt_value;
}

let strings = [40; 45; 50; 55; 59; 64]

let render_measure f tracks m =
   let track n = F.channel_owner n.channel f |> Option.get in
   let ngroups =
      List.enum m.notes
      |> Enum.filter (fun n -> List.mem (track n) tracks)
      |> Enum.group (fun n -> n.stime)
   in
   let column = Hashtbl.create 20 in
   let x = ref (0., 0.) in
   let stridx = Hashtbl.create 16 in
   List.iteri (fun i s -> Hashtbl.add stridx s i) strings;
   Enum.map (fun notes ->
      Enum.iter (fun n ->
         assert (n.str >= 0);
         let tr = track n in
         let q =
            try Hashtbl.find column (tr, n.str)
            with _ -> Queue.create () |> tap (Hashtbl.add column (tr, n.str))
         in
         Queue.push n q
      ) notes;
      let x0 = !x in
      x := !x +: (0., 1.);
      Hashtbl.enum column |> Enum.map (fun ((tr, str), q) ->
         let dx = ref 0 in
         let note_elt n =
            let txt =
               let s = Printf.sprintf "%i" (n.midipitch - n.str) in
               if !dx > 0 then "," ^ s else s
            in
            let elt = {
               x = x0 +: (float !dx, 0.);
               y = Hashtbl.find stridx n.str;
               track = tr;
               text = txt;
               value = `Nothing;
            } in
            dx := !dx + String.length txt;
            x := tabx_max !x (x0 +: (float !dx, 0.));
            elt
         in
         Queue.enum q /@ note_elt
      ) |> Enum.flatten |> tap Enum.force
   ) ngroups |> Enum.flatten

(* vim: set ts=3 sw=3 tw=80 : *)
