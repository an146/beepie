type midipitch = int
type velocity = int
type controller_type = int
type controller_value = int
type pitchwheel_value = int
type metaevent_type = controller_type

type voice_event =
     NoteOff         of midipitch * velocity
   | NoteOn          of midipitch * velocity
   | NoteAftertouch  of midipitch * velocity
   | Controller      of controller_type * controller_value
   | Program         of controller_value
   | ChannelPressure of velocity
   | PitchWheel      of pitchwheel_value

type meta_event =
     EndOfTrack
   | TrackName of string
   | UnknownMetaEvent of metaevent_type * string

type event =
     VoiceEvent of int * voice_event
   | MetaEvent of meta_event

let read_byte stream =
   int_of_char (Stream.next stream);;

let read_data_byte stream =
   let b = read_byte stream in
   if b land 0x80 != 0 then
      failwith "unexpected command byte";
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

let parse_voice_event code stream =
   let byte () = read_data_byte stream in
   match code with
       0x80 -> NoteOff         (byte (), byte ())
     | 0x90 -> NoteOn          (byte (), byte ())
     | 0xA0 -> NoteAftertouch  (byte (), byte ())
     | 0xB0 -> Controller      (byte (), byte ())
     | 0xC0 -> Program         (byte ())
     | 0xD0 -> ChannelPressure (byte ())
     | 0xE0 -> PitchWheel      (byte () * byte ())
     | _    -> assert false;;

let parse_meta_event stream =
   let mtype = read_data_byte stream in
   let len = read_varlen stream in
   let data = read_bytes len stream in
   match mtype with
       0x2F -> EndOfTrack
     | _ -> UnknownMetaEvent (mtype, data);;

let parse_event stream =
   let first_byte = read_byte stream in
   if first_byte < 0x80 then
      failwith "unexpected data byte"
   else if first_byte < 0xF0 then
      let code = first_byte land 0xF0 in
      let channel = first_byte land 0xF in
      VoiceEvent (channel, parse_voice_event code stream)
   else if first_byte = 0xFF then
      MetaEvent (parse_meta_event stream)
   else
      failwith "sysex events unsupported at the moment";;

let parse_stream stream =
   let converter _ =
      match Stream.peek stream with
        None -> None
      | _ -> Some (read_varlen stream, parse_event stream)
   in
   Stream.from converter

(* vim: set ts=3 sw=3 tw=80 : *)
