open MidiCmd
open MidiFile

let off c a b         = Voice (c, NoteOff         (a, b))
let on c a b          = Voice (c, NoteOn          (a, b))
let aftertouch c a b  = Voice (c, NoteAftertouch  (a, b))
let ctrl c a b        = Voice (c, Controller      (a, b))
let program c a       = Voice (c, Program         a)
let chpressure c a    = Voice (c, ChannelPressure a)
let pitchwheel c a    = Voice (c, PitchWheel      a)
let pitchwheel2 c a b = Voice (c, PitchWheel      (a * 0x80 + b))

let note channel midipitch (on_time, on_vel) (off_time, off_vel) = {
   channel; midipitch;
   on_time; on_vel;
   off_time; off_vel;
}

(* vim: set ts=3 sw=3 tw=80 : *)
