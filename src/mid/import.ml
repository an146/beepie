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

let import_cmd_stream file stream =
   let track = file#add_track () in
   ignore track;
   let process_cmd (dtime, event) =
      match event with
          MidiCmd.NoteOff (c, n, v) -> Printf.printf "off%i %i %i\n" c n v
        | MidiCmd.NoteOn  (c, n, v) -> Printf.printf "on%i %i %i\n" c n v
        | _ -> ()
   in
   Stream.iter process_cmd stream;;

let import_track file track_s track_s_offset =
   import_cmd_stream file (MidiCmd.parse_stream track_s track_s_offset);;

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

let do_import channel =
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
   for i = 1 to tracks do
      let track_s, track_s_offset = get_chunk "MTrk" channel in
      import_track file track_s track_s_offset
   done;
   file;;

let import_inline ?(division = 240) tracks =
   let file = new MidiFile.file division in
   let import_inline_track track =
      import_cmd_stream file (Stream.of_list track)
   in
   List.iter import_inline_track tracks;
   file;;

let import filename =
   let channel = open_in_bin filename in
   let file =
      try do_import channel
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
