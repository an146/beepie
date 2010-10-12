open Base
open IntX

let read_int n stream =
   let ret = ref 0 in
   for i = 1 to n do
      let b = int_of_char (Stream.next stream) in
      ret := !ret * 256 + b
   done;
   !ret;;

let read_word = read_int 2;;

let bigendian_to_int str =
   read_int (String.length str) (Stream.of_string str);;

let default_velocity on_vel =
   ignore on_vel;
   64;;

let read_chunk_header channel =
   let magic = String.create 4 in
   let length = String.create 4 in
   really_input channel magic 0 4;
   really_input channel length 0 4;
   (magic, bigendian_to_int length);;

(* в рот мне ноги! *)
exception Unexpected_Magic

let rec get_chunk ?(really_expect = false) expected_magic channel =
   let magic, length = read_chunk_header channel in
   let chunk_offset = pos_in channel in
   if magic = expected_magic then
      let chunk = String.create length in
      really_input channel chunk 0 length;
      (Stream.of_string chunk, chunk_offset)
   else begin (* magic != expected_magic *)
      if really_expect then
         raise Unexpected_Magic;
      let new_pos = (pos_in channel) + length in
      seek_in channel new_pos;
      get_chunk expected_magic channel
   end;;

let really_get_chunk = get_chunk ~really_expect: true

let do_import file nTracks get_track =
   let track_streams = Array.init nTracks (fun _ -> get_track ()) in
   let order a b = MidiCmd.stream_order track_streams.(a) track_streams.(b) in
   let queue = PriorityQueue.make order in
   for i = 0 to nTracks - 1 do
      file#add_track ();
      PriorityQueue.add queue i
   done;
   let get_event _ =
      try
         let track_id = PriorityQueue.first queue in
         let time, ev = Stream.next track_streams.(track_id) in
         PriorityQueue.reorder_down queue track_id;
         Some (track_id, time, ev)
      with Stream.Failure -> None
   in
   let notes = Array.init 16 (fun i -> ignore i; Array.make 128 None) in
   let off c n off_time off_vel =
      let n7 = int7_of_int n in
      match notes.(c).(n) with
      | None -> ()
      | Some (track, on_time, on_vel) ->
            let off_vel =
               if off_vel >= 0 then
                  off_vel
               else
                  default_velocity on_vel
            in
            let e1 = (on_time, int7_of_int on_vel) in
            let e2 = (off_time, int7_of_int off_vel) in
            file#insert_note track (int4_of_int c) n7 e1 e2;
            notes.(c).(n) <- None
   in
   let handle_event (track, time, ev) =
      match ev with
      | MidiCmd.NoteOn (Int4 c, Int7 n, Int7 v) ->
            off c n time (-1);
            notes.(c).(n) <- Some (track, time, v)
      | MidiCmd.NoteOff (Int4 c, Int7 n, Int7 v) ->
            off c n time v
      | _ -> ()
   in
   Stream.iter handle_event (Stream.from get_event);;

let import_io_channel channel =
   let header_s, _ =
      try really_get_chunk "MThd" channel
      with Unexpected_Magic -> failwith "not a MIDI file"
   in
   let fmt = read_word header_s in
   if fmt != 1 then
      failwith "unsupported MIDI format";
   let tracks = read_word header_s in
   let division = read_word header_s in
   let file = new MidiFile.file division in
   let get_track () =
      let track_s, track_s_offset = get_chunk "MTrk" channel in
      MidiCmd.parse_stream track_s track_s_offset
   in
   do_import file tracks get_track;
   file;;

let import_inline ?(division = 240) tracks =
   let file = new MidiFile.file division in
   let tracks = ref tracks in
   let get_track () =
      let track = Stream.of_list (List.hd !tracks) in
      tracks := List.tl !tracks;
      track
   in
   do_import file (List.length !tracks) get_track;
   file;;

let import filename =
   let channel = open_in_bin filename in
   let file =
      try import_io_channel channel
      with e ->
         close_in channel;
         match e with
           End_of_file -> failwith "unexpected end of file"
         | e -> raise e
   in
   close_in channel;
   file#set_filename filename;
   file;;

(* vim: set ts=3 sw=3 tw=80 : *)
