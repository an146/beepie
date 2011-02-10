open MidiNote

let off c a b         = `NoteOff         (c, a, b)
let on c a b          = `NoteOn          (c, a, b)
let aftertouch c a b  = `NoteAftertouch  (c, a, b)
let ctrl c a b        = `Controller      (c, a, b)
let program c a       = `Program         (c, a)
let chpressure c a    = `ChannelPressure (c, a)
let pitchwheel c a    = `PitchWheel      (c, a)

let pitchwheel2 c a b = pitchwheel c (a * 0x80 + b)
let noteoff (c, n)    = off c n.midipitch n.off_vel
let noteon (c, n)     = on c n.midipitch n.on_vel

let ctrl2 ch ctrltype v =
   match ctrltype with
   | Ctrl.Program -> program ch v
   | Ctrl.PitchWheel -> pitchwheel ch v
   | Ctrl.Controller c -> ctrl ch c v

let note midipitch (on_time, on_vel) (off_time, off_vel) = {
   midipitch;
   on_time; on_vel;
   off_time; off_vel;
}

let tempo t = `Tempo t
let timesig n d = `TimeSig (TimeSig.create n d)

(* vim: set ts=3 sw=3 tw=80 : *)
