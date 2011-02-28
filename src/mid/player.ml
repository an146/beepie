open Batteries
open MidiAsm
open React
module File = MidiFile

let miditime_of_time t tempo d =
   int_of_float (t *. 1000000.0 *. (float_of_int d) /. (float_of_int tempo))

let time_of_miditime mt tempo d =
   (float_of_int mt) *. (float_of_int tempo) /. (float_of_int d) /. 1000000.0

type ctx = {
   file : MidiFile.t;
   events : (int * int * MidiCmd.t) Enum.t;
   mutable pivot_time : float;
   mutable pivot_miditime : int;
   mutable pivot_tempo : int;
}

let ctx = ref None

let process () =
   Option.may (fun c ->
      let div = File.division c.file in
      let dt = Unix.gettimeofday () -. c.pivot_time in
      let dtime = miditime_of_time dt c.pivot_tempo div in
      let flt (t, _, _) = t <= c.pivot_miditime + dtime in
      Enum.take_while flt c.events |> Enum.iter (fun (t, tr, cmd) ->
         match cmd with
         | `Tempo tempo ->
               c.pivot_time <- c.pivot_time +. (
                  time_of_miditime (t - c.pivot_miditime) c.pivot_tempo div
               );
               c.pivot_miditime <- t;
               c.pivot_tempo <- tempo
         | _ -> ();
         MidiIo.output (MidiCmd.to_string cmd)
      );
      MidiIo.flush_output ()
   ) !ctx

let file () = Option.map (fun {file} -> file) !ctx

let file_signal, update_file = S.create ~eq:(==) None

let reset_output () =
   for i = 0 to 15 do
      program i 0 |> MidiCmd.to_string |> MidiIo.output;
      ctrl2 i Ctrl.all_controllers_off 0 |> MidiCmd.to_string |> MidiIo.output
   done

let set_ctx ctx' =
   ctx := ctx';
   update_file (file ())

let play f =
   if Option.is_some (file ()) then
      failwith "already playing";
   reset_output ();
   set_ctx (Some {
      file = f;
      events = Export.export_events f;
      pivot_time = Unix.gettimeofday ();
      pivot_miditime = 0;
      pivot_tempo = CtrlMap.get 0 (File.tempo_map f);
   })

let stop () =
   set_ctx None;
   reset_output ();
   for i = 0 to 15 do
      ctrl2 i Ctrl.all_notes_off 0 |> MidiCmd.to_string |> MidiIo.output
   done;
   MidiIo.flush_output ()

(* vim: set ts=3 sw=3 tw=80 : *)
