open Batteries
open IO
open BigEndian
open MidiAsm
open MidiFile
open MidiNote
open Varlen

type chunk = {
   magic  : string;
   input  : input;
   offset : unit -> int;
}

let parse_track_chunk chunk =
   let running_status = ref (-1) in
   let time = ref 0 in
   let get_time delta =
      time := !time + delta;
      !time
   in
   let get_event () =
      try
         let dtime = read_varlen chunk.input in
         let cmd = MidiCmd.read ~running_status chunk.input in
         Some (get_time dtime, cmd)
      with No_more_input -> None
   in
   Enum.from_while get_event

let default_velocity svel =
   ignore svel;
   64

let parse_chunks (input, inoffset) =
   let read_ui32 input =
      let n = read_i32 input in
      if n < 0 then failwith "overflow";
      n
   in
   let get_chunk () =
      try
         let magic = really_nread input 4 in
         let length = read_ui32 input in
         let start_offset = inoffset () in
         let content = really_nread input length in
         let cinput, coffset = pos_in (input_string content) in
         Some {
            magic = magic;
            input = cinput;
            offset = fun () -> start_offset + coffset ();
         }
      with No_more_input -> None
   in
   Enum.from_while get_chunk

let import_events ?(division = 240) ?(appl = true) events =
   let file = ref (F.create division) in
   let notes = Array.init 16 (fun _ -> Array.make 128 None) in
   let off channel midipitch etime evel =
      match notes.(channel).(midipitch) with
      | None -> ()
      | Some (tn, stime, svel) ->
            let evel =
               if evel >= 0 then
                  evel
               else
                  default_velocity svel
            in
            let note = {
               channel;
               midipitch;
               stime; svel;
               etime; evel;
               str = -1;
            } in
            file := F.add_note (F.track tn !file) note !file;
            notes.(channel).(midipitch) <- None
   in
   let ctrl c t time v =
      if Ctrl.is_supported t then (
         let map = F.ctrl_map (c, t) !file |> CtrlMap.set time v in
         file := F.set_ctrl_map (c, t) map !file;
      );
   in
   let unhandled = ref 0 in
   let first_track_used = ref false in
   let handle_event (time, track, ev) =
      if track < 0 then
         first_track_used := true;
      while F.tracks_count !file <= track do
         file := F.add_track !file
      done;
      match ev with
      | `NoteOn (c, n, v) ->
            if track = 0 then
               first_track_used := true;
            off c n time (-1);
            notes.(c).(n) <- Some (track, time, v)
      | `NoteOff (c, n, v) ->
            off c n time v
      | `Controller (c, t, v) ->
            ctrl c (Ctrl.Controller t) time v
      | `Program (c, v) ->
            ctrl c Ctrl.Program time v
      | `PitchWheel (c, v) ->
            ctrl c Ctrl.PitchWheel time v
      | `TrackName s ->
            file := F.set_track_name s (!file, F.track track !file)
      | `EndOfTrack ->
            (* TODO: off all unoffed notes *)
            ()
      | `Tempo v ->
            let map = F.tempo_map !file |> CtrlMap.set time v in
            file := F.set_tempo_map map !file
      | `TimeSig ts ->
            let map = F.timesig_map !file |> CtrlMap.set time ts in
            file := F.set_timesig_map map !file
      | _ ->
            unhandled := !unhandled + 1
   in
   Enum.iter handle_event events;
   if !unhandled > 0 then
      Printf.printf "%i events unhandled\n%!" !unhandled;
   let head_track = F.track 0 !file in
   if Enum.is_empty (F.channels head_track !file) then
      file := F.remove_track head_track !file;
   if appl then (
      Enum.iter (fun t ->
         Applicature.update (!file, t) [40; 45; 50; 55; 59; 64]
      ) (F.tracks !file)
   );
   !file

let import_events ?division ?appl t =
   try import_events ?division ?appl t
   with End_of_file -> failwith "unexpected end of file"

let import_events2 ?division ?appl tracks =
   let interleaved =
      let f i e = e /@ fun (t, c) -> t, i, c in
      tracks |> Enum.mapi f |> MiscUtils.enum_merge2 compare
   in
   import_events ?division ?appl interleaved

let import_input input =
   let chunks = parse_chunks (pos_in input) in
   let header =
      match Enum.get chunks with
      | Some h when h.magic = "MThd" -> h
      | _ -> failwith "not a MIDI file"
   in
   let fmt = read_ui16 header.input in
   if fmt != 1 then
      failwith "unsupported MIDI format";
   let tracks_count = read_ui16 header.input in
   ignore tracks_count;
   let division = read_ui16 header.input in
   let tracks = chunks // (fun c -> c.magic = "MTrk") /@ parse_track_chunk in
   import_events2 ~division tracks

let import_inline ?division ?appl tracks =
   let tracks = List.enum tracks /@ List.enum in
   import_events2 ?division ?appl tracks

let import_file filename =
   let c = open_in_bin filename in
   finally (fun () -> close_in c) import_input c

(* vim: set ts=3 sw=3 tw=80 : *)
