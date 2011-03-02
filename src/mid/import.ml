open Batteries
open IO
open BigEndian
open MidiAsm
open MidiNote
open Varlen
module File = MidiFile

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

let default_velocity on_vel =
   ignore on_vel;
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

let import_events ?(division = 240) events =
   let file = ref (File.create division) in
   let notes = Array.init 16 (fun _ -> Array.make 128 None) in
   let off channel midipitch off_time off_vel =
      match notes.(channel).(midipitch) with
      | None -> ()
      | Some (tn, on_time, on_vel) ->
            let off_vel =
               if off_vel >= 0 then
                  off_vel
               else
                  default_velocity on_vel
            in
            let note = {
               midipitch;
               on_time; on_vel;
               off_time; off_vel;
               str = 0;
            } in
            file := File.add_note ~channel (File.track tn !file) note !file;
            notes.(channel).(midipitch) <- None
   in
   let ctrl c t time v =
      if Ctrl.is_supported t then (
         let map = File.ctrl_map (c, t) !file |> CtrlMap.set time v in
         file := File.set_ctrl_map (c, t) map !file;
      );
   in
   let unhandled = ref 0 in
   let first_track_used = ref false in
   let handle_event (time, track, ev) =
      if track < 0 then
         first_track_used := true;
      while File.tracks_count !file <= track do
         file := File.add_track !file
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
      | `Tempo v ->
            let map = File.tempo_map !file |> CtrlMap.set time v in
            file := File.set_tempo_map map !file
      | `TimeSig ts ->
            let map = File.timesig_map !file |> CtrlMap.set time ts in
            file := File.set_timesig_map map !file
      | _ ->
            unhandled := !unhandled + 1
   in
   Enum.iter handle_event events;
   if !unhandled > 0 then
      Printf.printf "%i events unhandled\n%!" !unhandled;
   let head_track = File.track 0 !file in
   if Enum.is_empty (File.channels head_track !file) then
      file := File.remove_track head_track !file;
   !file

let import_events ?division t =
   try import_events ?division t
   with End_of_file -> failwith "unexpected end of file"

let import_events2 ?division tracks =
   let interleaved =
      let f i e = e /@ fun (t, c) -> t, i, c in
      tracks |> Enum.mapi f |> MiscUtils.enum_merge2 compare
   in
   import_events ?division interleaved

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

let import_inline ?division tracks =
   let tracks = List.enum tracks /@ List.enum in
   import_events2 ?division tracks

let import_file filename =
   let c = open_in_bin filename in
   finally (fun () -> close_in c) import_input c

(* vim: set ts=3 sw=3 tw=80 : *)
