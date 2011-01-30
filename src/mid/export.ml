open MidiAsm
open MidiFile
open MiscUtils

type channel_ctx = {
   track : int;
   mutable ons : int;
   mutable ctrls_current : (Ctrl.t, int) PMap.t;
   mutable ctrls_cached : (Ctrl.t, int) PMap.t;
}

type event =
   (* order matters, the priority of events is determined by compare *)
   | Off of (int * note)
   | Ctrl of int * Ctrl.t * (int * int)
   | On of (int * note)

let event_time e =
   match e with
   | Off (_, n) -> n.off_time
   | Ctrl (_, _, (time, _)) -> time
   | On (_, n) -> n.on_time

let export_events file =
   let cctxs =
      let ctrls_current =
         let entry c = c, Ctrl.default_value c in
         Ctrl.all_supported |> List.enum |> Enum.map entry |> PMap.of_enum
      in
      let ctx track = {
         track;
         ons = 0;
         ctrls_current;
         ctrls_cached = PMap.empty
      } in
      Array.init 16 (fun i -> File.channel_owner i file |> Option.map ctx)
   in
   let owned i = Option.is_some cctxs.(i)
   and cctx i =
      match cctxs.(i) with
      | None -> failwith "unowned channel"
      | Some ctx -> ctx
   in
   let e_ons =
      let e_track track = File.enum_notes ~track file /@ fun n -> On n in
      File.tracks file /@ e_track
   and e_ctrls =
      let all_channels = 0 -- 15 |> List.of_enum in
      let ctrls = List.cartesian_product all_channels Ctrl.all_supported in
      let e_ctrl (ch, ct) =
         let ctrlmap = File.ctrlmap (ch, ct) file in
         CtrlMap.enum ctrlmap /@ fun c -> Ctrl (ch, ct, c)
      in
      List.enum ctrls // (fun (i, _) -> owned i) /@ e_ctrl
   in
   let e_cmp e1 e2 =
      match Enum.peek e1, Enum.peek e2 with
      | Some v1, Some v2 -> compare (event_time v1, v1) (event_time v2, v2)
      | _, _ -> assert false
   in
   let heap = BinaryHeap.make e_cmp (16 * (128 + 1 + 1)) in
   let not_empty e = not (Enum.is_empty e) in
   e_ons // not_empty |> Enum.iter (BinaryHeap.push heap);
   e_ctrls // not_empty |> Enum.iter (BinaryHeap.push heap);
   let get_events () =
      if BinaryHeap.is_empty heap then raise Enum.No_more_elements;
      let e = BinaryHeap.top heap in
      let ev = Enum.get e in
      if Enum.is_empty e then
         BinaryHeap.drop heap
      else
         BinaryHeap.reorder_top heap;
      match ev with
      | None -> assert false
      | Some (Off (c, n)) ->
            let ctx = cctx c in
            ctx.ons <- ctx.ons - 1;
            [n.off_time, ctx.track, noteoff (c, n)]
      | Some (Ctrl (ch, ct, (t, v))) ->
            let ctx = cctx ch in
            let cur_v = PMap.find ct ctx.ctrls_current in
            if v = cur_v then (
               ctx.ctrls_cached <- PMap.remove ct ctx.ctrls_cached;
               []
            ) else if ctx.ons <= 0 then (
               ctx.ctrls_cached <- PMap.add ct v ctx.ctrls_cached;
               []
            ) else (
               ctx.ctrls_current <- PMap.add ct v ctx.ctrls_current;
               [t, ctx.track, ctrl2 ch ct v]
            )
      | Some (On (c, n)) ->
            let ctx = cctx c in
            let time, track = n.on_time, ctx.track in
            ctx.ons <- ctx.ons + 1;
            Enum.singleton (Off (c, n)) |> BinaryHeap.push heap;
            let oncmd = time, track, noteon (c, n) in
            if ctx.ons = 1 then
               let ctrl_cmd (ct, v) = time, track, ctrl2 c ct v in
               let ctrl_cmds = PMap.enum ctx.ctrls_cached /@ ctrl_cmd in
               List.append (List.of_enum ctrl_cmds) [oncmd]
            else
               [oncmd]
   in
   Enum.from get_events |> Enum.map List.enum |> Enum.flatten

let export_file file filename =
   let channel = open_out_bin filename in
   close_out channel;;

(* vim: set ts=3 sw=3 tw=80 : *)
