open Batteries
open IO
open BigEndian
open MidiAsm
open MidiCmd
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
               ctx.ctrls_cached <- PMap.empty;
               List.append (List.of_enum ctrl_cmds) [oncmd]
            else
               [oncmd]
   in
   Enum.from get_events |> Enum.map List.enum |> Enum.flatten

let write_varlen o n =
   let l = ref [n mod 0x80] in
   let n = ref (n / 0x80) in
   while !n > 0 do
      l := (!n mod 0x80 + 0x80) :: !l;
      n := !n / 0x80
   done;
   List.iter (write_byte o) !l

let export_output f out =
   nwrite out "MThd";
   write_i32 out 6;
   write_ui16 out 1;
   let ntracks = File.tracks_count f in
   write_ui16 out ntracks;
   write_ui16 out (File.division f);
   let tctxs = Array.init ntracks (fun _ -> 0, None, output_string ()) in
   let evs = export_events f in
   let process_event (time, track, cmd) =
      let prevtime, prevstatus, otrk = tctxs.(track) in
      let dtime = time - prevtime in
      assert (dtime >= 0);
      write_varlen otrk dtime;
      let code = function
         | NoteOff         _ -> 0x80
         | NoteOn          _ -> 0x90
         | NoteAftertouch  _ -> 0xA0
         | Controller      _ -> 0xB0
         | Program         _ -> 0xC0
         | ChannelPressure _ -> 0xD0
         | PitchWheel      _ -> 0xE0
      and write_args o = function
         | NoteOff (a, b)
         | NoteOn (a, b)
         | NoteAftertouch (a, b)
         | Controller (a, b) ->
               write_byte o a;
               write_byte o b
         | Program a
         | ChannelPressure a ->
               write_byte o a
         | PitchWheel v ->
               write_byte o (v mod 0x80);
               write_byte o (v / 0x80)
      in
      let status =
         match cmd with
         | Voice (c, cmd) ->
               let st = (code cmd) + c in
               if prevstatus <> Some st; then
                  write_byte otrk st;
               write_args otrk cmd;
               Some st
         | Meta (t, s) ->
               write_byte otrk 0xFF;
               write_byte otrk t;
               write_varlen otrk (String.length s);
               nwrite otrk s;
               None
      in
      tctxs.(track) <- time, status, otrk;
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
