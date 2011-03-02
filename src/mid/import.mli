open Batteries
open MidiFile

val default_velocity : int -> int

val import_inline :
   ?division:int ->
   ?appl:bool ->
   (int * MidiCmd.t) list list ->
   file

val import_events :
   ?division:int ->
   ?appl:bool ->
   (int * int * MidiCmd.t) Enum.t ->
   file

val import_file : string -> file

(* vim: set ts=3 sw=3 tw=80 : *)
