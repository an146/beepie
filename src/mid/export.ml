open Batteries
open IO
open BigEndian
open MidiAsm
open MidiFile
open MidiNote
open MiscUtils
open Varlen

type channel_ctx = {
   track : int;
   mutable ons : int;
   mutable ctrls_current : (Ctrl.t, int) PMap.t;
   mutable ctrls_cached : (Ctrl.t, int) PMap.t;
}

type event =
   (* order matters, the priority of events is determined by compare *)
   | HeadTrackCmd of (int * MidiCmd.t)
   | Off of note
   | Ctrl of int * Ctrl.t * (int * int)
   | On of note

let event_time e =
   match e with
   | HeadTrackCmd (time, _) -> time
   | Off n -> n.etime
   | Ctrl (_, _, (time, _)) -> time
   | On n -> n.stime

let export_events file =
   let cctxs =
      let ctrls_current =
         let entry c = c, Ctrl.default_value c in
         Ctrl.all_supported |> List.enum |> Enum.map entry |> PMap.of_enum
      in
      let ctx tr = {
         track = F.track_index (file, tr);
         ons = 0;
         ctrls_current;
         ctrls_cached = PMap.empty
      } in
      Array.init 16 (fun i -> F.channel_owner i file |> Option.map ctx)
   in
   let owned i = Option.is_some cctxs.(i)
   and cctx i =
      match cctxs.(i) with
      | None -> failwith "unowned channel"
      | Some ctx -> ctx
   in
   let e_ons =
      let e_track tr = F.enum_notes ~track:tr file /@ fun n -> On n in
      F.tracks file /@ e_track
   and e_ctrls =
      let all_channels = 0 -- 15 |> List.of_enum in
      let ctrls = List.cartesian_product all_channels Ctrl.all_supported in
      let e_ctrl (ch, ct) =
         let ctrl_map = F.ctrl_map (ch, ct) file in
         CtrlMap.enum ctrl_map /@ fun c -> Ctrl (ch, ct, c)
      in
      List.enum ctrls // (fun (i, _) -> owned i) /@ e_ctrl
   and e_tempo =
      let cmd (t, v) = HeadTrackCmd (t, tempo v) in
      F.tempo_map file |> CtrlMap.enum |> Enum.map cmd
   and e_timesig =
      let cmd (t, v) = HeadTrackCmd (t, `TimeSig v) in
      F.timesig_map file |> CtrlMap.enum |> Enum.map cmd
   in
   let event_track = function
      | HeadTrackCmd (time, _) ->
            -1
      | Off {channel = c}
      | Ctrl (c, _, _)
      | On {channel = c} ->
            (cctx c).track
   in
   let e_cmp e1 e2 =
      match Enum.peek e1, Enum.peek e2 with
      | Some ev1, Some ev2 ->
            let values ev = event_time ev, event_track ev, ev in
            compare (values ev1) (values ev2)
      | _, _ ->
            assert false
   in
   let heap = BinaryHeap.make e_cmp (16 * (128 + 1 + 1)) in
   let not_empty e = not (Enum.is_empty e) in
   let e_all =
      [
         e_ons;
         e_ctrls;
         Enum.singleton e_tempo;
         Enum.singleton e_timesig;
      ] |> List.enum |> Enum.flatten |> Enum.filter not_empty
   in
   Enum.iter (BinaryHeap.push heap) e_all;
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
      | Some (Off n) ->
            let ctx = cctx n.channel in
            ctx.ons <- ctx.ons - 1;
            [n.etime, ctx.track, noteoff n]
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
      | Some (On n) ->
            let ctx = cctx n.channel in
            let time, track = n.stime, ctx.track in
            ctx.ons <- ctx.ons + 1;
            Enum.singleton (Off n) |> BinaryHeap.push heap;
            let oncmd = time, track, noteon n in
            if ctx.ons = 1 then
               let ctrl_cmd (ct, v) = time, track, ctrl2 n.channel ct v in
               let ctrl_cmds = PMap.enum ctx.ctrls_cached /@ ctrl_cmd in
               ctx.ctrls_cached <- PMap.empty;
               List.append (List.of_enum ctrl_cmds) [oncmd]
            else
               [oncmd]
      | Some (HeadTrackCmd (time, cmd)) ->
            [time, -1, cmd]
   in
   Enum.from get_events |> Enum.map List.enum |> Enum.flatten

let export_output f out =
   nwrite out "MThd";
   write_i32 out 6;
   write_ui16 out 1;
   let ntracks = F.tracks_count f + 1 in
   write_ui16 out ntracks;
   write_ui16 out (F.division f);
   let tctxs =
      let tctx _ = 0, ref (-1), output_string () in
      Array.init ntracks tctx
   in
   let evs = export_events f in
   let process_event (time, track, cmd) =
      let track = track + 1 in
      let prevtime, running_status, otrk = tctxs.(track) in
      let dtime = time - prevtime in
      assert (dtime >= 0);
      write_varlen otrk dtime;
      MidiCmd.write ~running_status otrk cmd;
      tctxs.(track) <- time, running_status, otrk;
   in
   Enum.iter process_event evs;
   let write_track (_, _, otrk) =
      nwrite otrk "\x00\xFF\x2F\x00";
      let trk = close_out otrk in
      nwrite out "MTrk";
      write_i32 out (String.length trk);
      nwrite out trk
   in
   Array.iter write_track tctxs

let export_file file filename =
   let o = open_out_bin filename in
   finally (fun () -> close_out o) (export_output file) o

(* vim: set ts=3 sw=3 tw=80 : *)
