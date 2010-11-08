open IntX
open MidiCmd

let off c a b        = NoteOff         (int4_of_int c, int7_of_int a, int7_of_int b)
let on c a b         = NoteOn          (int4_of_int c, int7_of_int a, int7_of_int b)
let aftertouch c a b = NoteAftertouch  (int4_of_int c, int7_of_int a, int7_of_int b)
let ctrl c a b       = Controller      (int4_of_int c, int7_of_int a, int7_of_int b)
let program c a      = Program         (int4_of_int c, int7_of_int a)
let chpressure c a   = ChannelPressure (int4_of_int c, int7_of_int a)
let pitchwheel c a   = PitchWheel      (int4_of_int c, int14_of_int a)

(* vim: set ts=3 sw=3 tw=80 : *)
