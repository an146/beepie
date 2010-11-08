open IntX
open MidiTypes

type midipitch = int7
type velocity = int7
type controller_type = int7
type controller_value = int7
type pitchwheel_value = int14
type metaevent_type = controller_type
type channel = int4

type t =
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

(* vim: set ts=3 sw=3 tw=80 : *)
