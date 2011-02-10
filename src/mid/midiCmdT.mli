type voice_t = [
   | `NoteOff         of int * int * int
   | `NoteOn          of int * int * int
   | `NoteAftertouch  of int * int * int
   | `Controller      of int * int * int
   | `Program         of int * int
   | `ChannelPressure of int * int
   | `PitchWheel      of int * int
] and meta_t = [
   | `Tempo of int
   | `TimeSig of TimeSig.t
   | `UnsupportedMeta of int * string
]

type t = [voice_t | meta_t]

(* vim: set ts=3 sw=3 tw=80 : *)
