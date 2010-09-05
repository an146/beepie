type midipitch = int
type velocity = int
type controller_type = int
type controller_value = int
type pitchwheel_value = int
type metaevent_type = controller_type
type channel = int

type event =
   (* Voice *)
     NoteOff         of channel * midipitch * velocity
   | NoteOn          of channel * midipitch * velocity
   | NoteAftertouch  of channel * midipitch * velocity
   | Controller      of channel * controller_type * controller_value
   | Program         of channel * controller_value
   | ChannelPressure of channel * velocity
   | PitchWheel      of channel * pitchwheel_value

   (* Meta *)
   | EndOfTrack
   | TrackName of string
   | UnknownMetaEvent of metaevent_type * string

let peek_byte stream =
   match Stream.peek stream with
       Some c -> int_of_char c
     | None -> assert false;;

let read_byte stream =
   int_of_char (Stream.next stream);;

let byte_info stream s_offset =
   let byte = peek_byte stream in
   let offset = s_offset + Stream.count stream in
   Printf.sprintf "0x%02X at offset 0x%X" byte offset;;

let read_data_byte stream s_offset =
   let b = peek_byte stream in
   if b land 0x80 != 0 then
      failwith ("unexpected command byte " ^ (byte_info stream s_offset));
   Stream.junk stream;
   b;;

let read_bytes n stream =
   let ret = String.create n in
   for i = 0 to n - 1 do
      ret.[i] <- Stream.next stream
   done;
   ret;;

(* TODO: check for overflow *)
let rec read_varlen stream =
   let b = read_byte stream in
   let v = b land 0x7F in
   if b < 0x80 then
      v
   else begin
      let ret = v * 0x80 + (read_varlen stream) in
      assert (ret >= 0);
      ret
   end;;

let parse_voice_event status stream s_offset =
   let code = status land 0xF0 in
   let ch = status land 0xF in
   let byte () = read_data_byte stream s_offset in
   match code with
       0x80 -> NoteOff         (ch, byte (), byte ())
     | 0x90 -> NoteOn          (ch, byte (), byte ())
     | 0xA0 -> NoteAftertouch  (ch, byte (), byte ())
     | 0xB0 -> Controller      (ch, byte (), byte ())
     | 0xC0 -> Program         (ch, byte ())
     | 0xD0 -> ChannelPressure (ch, byte ())
     | 0xE0 -> PitchWheel      (ch, byte () * byte ())
     | _    -> assert false;;

let parse_meta_event stream s_offset =
   let mtype = read_data_byte stream s_offset in
   let len = read_varlen stream in
   let data = read_bytes len stream in
   match mtype with
       0x2F -> EndOfTrack
     | _ -> UnknownMetaEvent (mtype, data);;

let parse_event stream s_offset running_status =
   let status =
      let b = peek_byte stream in
      if b >= 0x80 then (
         Stream.junk stream;
         b
      ) else if running_status >= 0x80 then (
         running_status
      ) else
         failwith ("unexpected data byte " ^ (byte_info stream s_offset))
   in
   if status < 0xF0 then
      parse_voice_event status stream s_offset, status
   else if status = 0xFF then
      parse_meta_event stream s_offset, -1
   else
      failwith "sysex events unsupported at the moment";;

let read_event stream s_offset running_status =
   let time = read_varlen stream in
   let event, new_rs = parse_event stream s_offset running_status in
   (time, event, new_rs);;

let parse_stream stream s_offset =
   let running_status = ref (-1) in
   let converter _ =
      if Stream.peek stream = None then
         None
      else
         match read_event stream s_offset !running_status with
            time, ev, new_rs -> running_status := new_rs; Some (time, ev)
   in
   Stream.from converter

(* vim: set ts=3 sw=3 tw=80 : *)
