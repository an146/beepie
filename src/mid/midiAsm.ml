open MidiCmd
open MidiFile

let off c a b         = Voice (c, NoteOff         (a, b))
let on c a b          = Voice (c, NoteOn          (a, b))
let aftertouch c a b  = Voice (c, NoteAftertouch  (a, b))
let ctrl c a b        = Voice (c, Controller      (a, b))
let program c a       = Voice (c, Program         a)
let chpressure c a    = Voice (c, ChannelPressure a)
let pitchwheel c a    = Voice (c, PitchWheel      a)

let pitchwheel2 c a b = pitchwheel c (a * 0x80 + b)
let noteoff n         = off n.channel n.midipitch n.off_vel
let noteon n          = on n.channel n.midipitch n.on_vel

let ctrl2 ch ctrltype v =
   match ctrltype with
   | Ctrl.Program -> program ch v
   | Ctrl.PitchWheel -> pitchwheel ch v
   | Ctrl.Controller c -> ctrl ch c v

let note channel midipitch (on_time, on_vel) (off_time, off_vel) = {
   channel; midipitch;
   on_time; on_vel;
   off_time; off_vel;
}

(* vim: set ts=3 sw=3 tw=80 : *)
