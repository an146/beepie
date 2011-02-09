open Batteries
open IO
open BigEndian
open MidiAsm
open MidiCmdT
open Varlen

type t = MidiCmdT.t

let channel = function
   | `NoteOff         (c, _, _) -> c
   | `NoteOn          (c, _, _) -> c
   | `NoteAftertouch  (c, _, _) -> c
   | `Controller      (c, _, _) -> c
   | `Program         (c, _)    -> c
   | `ChannelPressure (c, _)    -> c
   | `PitchWheel      (c, _)    -> c

let read_data_byte input =
   let b = read_byte input in
   if b >= 0x80 then
      failwith "unexpected command byte";
   b

let read ?running_status input =
   let running_status = Option.default (ref (-1)) running_status in
   let first = read_byte input in
   if first < 0xF0 then
      let args = DynArray.make 3 in
      let status =
         if first >= 0x80 then (
            running_status := first;
            first
         ) else if !running_status >= 0x80 then (
            DynArray.add args first;
            !running_status
         ) else
            failwith "no running status";
      in
      let code    = status land 0xf0 in
      let channel = status land 0x0F in
      let nargs, cmd =
         let voice1 f = 1, fun c args ->
            f c (DynArray.get args 0)
         and voice2 f = 2, fun c args ->
            f c (DynArray.get args 0) (DynArray.get args 1)
         in
         match code with
         | 0x80 -> voice2 off
         | 0x90 -> voice2 on
         | 0xA0 -> voice2 aftertouch
         | 0xB0 -> voice2 ctrl
         | 0xC0 -> voice1 program
         | 0xD0 -> voice1 chpressure
         | 0xE0 -> voice2 pitchwheel2
         | _    -> assert false
      in
      while DynArray.length args < nargs do
         DynArray.add args (read_data_byte input)
      done;
      cmd channel args
   else if first = 0xFF then
      let mtype = read_data_byte input in
      let len = read_varlen input in
      let data = really_nread input len in
      running_status := -1;
      `UnsupportedMeta (mtype, data)
   else
      failwith "sysex events unsupported at the moment"

let write_voice ~running_status out cmd =
   let status, args =
      match cmd with
      | `NoteOff         (c, a, b) -> 0x80 + c, [a; b]
      | `NoteOn          (c, a, b) -> 0x90 + c, [a; b]
      | `NoteAftertouch  (c, a, b) -> 0xA0 + c, [a; b]
      | `Controller      (c, a, b) -> 0xB0 + c, [a; b]
      | `Program         (c, a)    -> 0xC0 + c, [a]
      | `ChannelPressure (c, a)    -> 0xD0 + c, [a]
      | `PitchWheel      (c, v)    -> 0xE0 + c, [v mod 0x80; v / 0x80]
   in
   if status >= 0x80 && status != !running_status then
      write_byte out status;
   List.iter (write_byte out) args;
   running_status := status

let write_meta ~running_status out cmd =
   let rec encode_meta_int ?(acc = []) v n =
      match n, v with
      | 0, 0 -> acc
      | 0, _ -> failwith "overflow"
      | n, _ -> encode_meta_int ~acc:(v mod 0x100 :: acc) (v / 0x100) (n - 1)
   in
   let write_meta_s t s =
      write_byte out 0xFF;
      write_byte out t;
      write_varlen out (String.length s);
      nwrite out s;
   in
   let write_meta_l t l =
      List.map char_of_int l |> String.of_list |> write_meta_s t
   in
   running_status := -1;
   match cmd with
   | `Tempo t ->
         write_meta_l 0x51 (encode_meta_int t 3)
   | `UnsupportedMeta (t, s) ->
         write_meta_s t s

let write ?running_status out cmd =
   let running_status = Option.default (ref (-1)) running_status in
   match cmd with
   | #voice_t as cmd ->
         write_voice ~running_status out cmd
   | #meta_t as cmd ->
         write_meta ~running_status out cmd

(* vim: set ts=3 sw=3 tw=80 : *)
