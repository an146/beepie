let read_int n stream =
   let ret = ref 0 in
   for i = 1 to n do
      let b = int_of_char (Stream.next stream) in
      ret := !ret * 256 + b
   done;
   !ret;;

let read_word = read_int 2;;

let bigendian_to_int s =
   read_int (String.length s) (Stream.of_string s);;

let import_track file track =
   ignore track;;

let read_chunk_header channel =
   let magic = String.create 4 in
   let length = String.create 4 in
   really_input channel magic 0 4;
   really_input channel length 0 4;
   (magic, bigendian_to_int length);;

exception Unexpected_Magic

let rec get_chunk ?(really_expect = false) expected_magic channel =
   let magic, length = read_chunk_header channel in
   if magic = expected_magic then
      let chunk = String.create length in
      really_input channel chunk 0 length;
      Stream.of_string chunk
   else begin (* magic != expected_magic *)
      if really_expect then
         raise Unexpected_Magic;
      let new_pos = (pos_in channel) + length in
      seek_in channel new_pos;
      get_chunk expected_magic channel
   end;;

let really_get_chunk = get_chunk ~really_expect: true

let do_import channel =
   let header =
      try really_get_chunk "MThd" channel
      with Unexpected_Magic -> failwith "not a MIDI file"
   in
   let fmt = read_word header in
   if fmt != 1 then
      failwith "unsupported MIDI format";
   let tracks = read_word header in
   let division = read_word header in
   let file = new Midifile.file division in
   for i = 1 to tracks do
      let track = get_chunk "MTrk" channel in
      import_track file track
   done;
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
