(* TODO: maybe expand Voice? *)
type t =
   | Voice of int * voice_t
   | Meta of int * string
and voice_t =
   | NoteOff         of int * int
   | NoteOn          of int * int
   | NoteAftertouch  of int * int
   | Controller      of int * int
   | Program         of int
   | ChannelPressure of int
   | PitchWheel      of int

(* vim: set ts=3 sw=3 tw=80 : *)
