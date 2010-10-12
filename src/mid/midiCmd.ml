open IntX

type midipitch = int7
type velocity = int7
type controller_type = int7
type controller_value = int7
type pitchwheel_value = int14
type metaevent_type = controller_type
type channel = int4
type miditime = int

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

let off c a b        = NoteOff         (int4_of_int c, int7_of_int a, int7_of_int b)
let on c a b         = NoteOn          (int4_of_int c, int7_of_int a, int7_of_int b)
let aftertouch c a b = NoteAftertouch  (int4_of_int c, int7_of_int a, int7_of_int b)
let ctrl c a b       = Controller      (int4_of_int c, int7_of_int a, int7_of_int b)
let program c a      = Program         (int4_of_int c, int7_of_int a)
let chpressure c a   = ChannelPressure (int4_of_int c, int7_of_int a)
let pitchwheel c a   = PitchWheel      (int4_of_int c, int14_of_int a)

let peek_byte stream =
   match Stream.peek stream with
   | Some c -> int_of_char c
   | None   -> failwith "peek_byte";;

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

let read_int14 stream s_offset =
   let b1 = read_data_byte stream s_offset in
   let b2 = read_data_byte stream (s_offset + 1) in
   int14_of_int (b1 * 128 + b2);;

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
   let int14 () = read_int14 stream s_offset in
   let voice1 cmd = cmd ch (byte ()) in
   let voice2 cmd =
      let arg1 = byte() in
      let arg2 = byte() in
      cmd ch arg1 arg2
   in
   match code with
   | 0x80 -> voice2 off
   | 0x90 -> voice2 on
   | 0xA0 -> voice2 aftertouch
   | 0xB0 -> voice2 ctrl
   | 0xC0 -> voice1 program
   | 0xD0 -> voice1 chpressure
   | 0xE0 -> PitchWheel (int4_of_int ch, int14 ())
   | _    -> assert false;;

let parse_meta_event stream s_offset =
   let mtype = read_data_byte stream s_offset in
   let len = read_varlen stream in
   let data = read_bytes len stream in
   match mtype with
   | 0x2F -> EndOfTrack
   | _    -> UnknownMetaEvent (int7_of_int mtype, data);;

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
   let dtime = read_varlen stream in
   let event, new_rs = parse_event stream s_offset running_status in
   (dtime, event, new_rs);;

let parse_stream stream s_offset =
   let running_status = ref (-1) in
   let time = ref 0 in
   let get_time delta =
      time := !time + delta;
      !time
   in
   let converter _ =
      if Stream.peek stream = None then
         None
      else
         match read_event stream s_offset !running_status with
            dtime, ev, new_rs ->
               running_status := new_rs; Some (get_time dtime, ev)
   in
   Stream.from converter;;

let stream_order s1 s2 =
   let a = Stream.peek s1 in
   let b = Stream.peek s2 in
   match a, b with
   | None, _ -> false
   | _, None -> true
   | Some (t1, _), Some (t2, _) -> t1 < t2;;

(* vim: set ts=3 sw=3 tw=80 : *)
