open IO
open BigEndian
open MidiAsm
open MidiCmd
open MidiFile

type chunk = {
   magic  : string;
   input  : input;
   offset : unit -> int;
}

let rec read_midi_byte is_cmd chunk =
   let offset = chunk.offset () in
   let b = read_byte chunk.input in
   if is_cmd != (b >= 0x80) then (
      failwith (Printf.sprintf "unexpected byte type at 0x%X" offset)
   );
   b
and read_cmd_byte  = fun c -> read_midi_byte true  c
and read_data_byte = fun c -> read_midi_byte false c

(* TODO: check for overflow *)
let rec parse_varlen input =
   let b = read_byte input in
   let v = b land 0x7F in
   if b < 0x80 then
      v
   else begin
      let ret = v * 0x80 + (parse_varlen input) in
      if ret < 0 then failwith "overflow";
      ret
   end

let parse_track_chunk chunk =
   let running_status = ref (-1) in
   let time = ref 0 in
   let get_time delta =
      time := !time + delta;
      !time
   in
   let get_event () =
      try
         let dtime = parse_varlen chunk.input in
         let first = read_byte chunk.input in
         let status, event =
            if first < 0xF0 then
               let status, args =
                  if first >= 0x80 then
                     first, ref [| |]
                  else if !running_status >= 0x80 then
                     !running_status, ref [| first |]
                  else
                     failwith "no running status";
               in
               let code    = status land 0xf0 in
               let channel = status land 0x0F in
               let voice1 f = 1, fun c args -> f c args.(0) in
               let voice2 f = 2, fun c args -> f c args.(0) args.(1) in
               let nargs, cmd =
                  match code with
                  | 0x80 -> voice2 off
                  | 0x90 -> voice2 on
                  | 0xA0 -> voice2 aftertouch
                  | 0xB0 -> voice2 ctrl
                  | 0xC0 -> voice1 program
                  | 0xD0 -> voice1 chpressure
                  | 0xE0 -> voice2 pitchwheel2
                  | _    -> assert false
               in
               while Array.length !args < nargs do
                  args := Array.append !args [| read_data_byte chunk |]
               done;
               status, cmd channel !args
            else if first = 0xFF then
               let mtype = read_data_byte chunk in
               let len = parse_varlen chunk.input in
               let data = really_nread chunk.input len in
               let event =
                  match mtype with
                  | 0x2F -> EndOfTrack
                  | _    -> UnknownMetaEvent (mtype, data)
               in
               -1, event
            else
               failwith "sysex events unsupported at the moment"
         in
         running_status := status;
         Some (get_time dtime, event)
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

let do_import file (tracks : (int * MidiCmd.t) Enum.t Enum.t) =
   let notes = Array.init 16 (fun _ -> Array.make 128 None) in
   let off channel midipitch off_time off_vel =
      match notes.(channel).(midipitch) with
      | None -> ()
      | Some (track, on_time, on_vel) ->
            let off_vel =
               if off_vel >= 0 then
                  off_vel
               else
                  default_velocity on_vel
            in
            let note = {
               channel; midipitch;
               on_time; on_vel;
               off_time; off_vel;
            } in
            file#insert_note track note;
            notes.(channel).(midipitch) <- None
   in
   let ctrl c t time v =
      if Ctrl.is_supported t then
         let channel = file#channel c in
         channel#ctrl t |> CtrlMap.set time v |> channel#set_ctrl t
   in
   let handle_event (track, (time, ev)) =
      match ev with
      | Voice (c, NoteOn (n, v)) ->
            off c n time (-1);
            notes.(c).(n) <- Some (track, time, v)
      | Voice (c, NoteOff (n, v)) ->
            off c n time v
      | Voice (c, Controller (t, v)) ->
            ctrl c (Ctrl.Controller t) time v
      | Voice (c, Program v) ->
            ctrl c Ctrl.Program time v
      | Voice (c, PitchWheel v) ->
            ctrl c Ctrl.PitchWheel time v
      | _ -> ()
   in
   tracks |> MiscUtils.enum_merge2i compare |> Enum.iter handle_event

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
   let division = read_ui16 header.input in
   let file = new MidiFile.file ~tracks_count division in
   let tracks = chunks // (fun c -> c.magic = "MTrk") /@ parse_track_chunk in
   do_import file tracks;
   file

let import_inline ?(division = 240) tracks =
   let tracks_count = List.length tracks in
   let file = new MidiFile.file ~tracks_count division in
   let tracks = List.enum tracks /@ List.enum in
   do_import file tracks;
   file

let import_file filename =
   let channel = open_in_bin filename in
   let file =
      try import_input channel
      with e ->
         close_in channel;
         match e with
         | End_of_file -> failwith "unexpected end of file"
         | e -> raise e
   in
   close_in channel;
   file#set_filename filename;
   file

(* vim: set ts=3 sw=3 tw=80 : *)
