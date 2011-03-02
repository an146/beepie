open Batteries
open MidiFile
open MidiNote

let weak_filter f e =
   let first = Enum.peek e |> Option.get in
   let flt = Enum.filter f e in
   if Enum.is_empty flt then
      Enum.singleton first
   else
      flt

let cartesian e e' =
   Enum.map (fun elt ->
      Enum.map (fun elt' -> elt, elt') e'
   ) e |> Enum.flatten

let update (file, track) strings =
   let string_choices note =
      List.enum strings |> weak_filter ((>=) note.midipitch)
   in
   let place apps (_, note) =
      let no_conflict ((a, fs), s) = List.mem s fs in
      let apply ((a, fs), s) = (note, s) :: a, List.filter ((!=) s) fs in
      cartesian apps (string_choices note)
            |> weak_filter no_conflict
            |> Enum.map apply
   in
   let init = Enum.singleton ([], strings) in
   let notes = F.enum_notes ~track file in
   let a = Enum.fold place init notes |> Enum.get |> Option.get in
   List.iter (fun (n, s) -> n.str <- s) (fst a)

(* vim: set ts=3 sw=3 tw=80 : *)
