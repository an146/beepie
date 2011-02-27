open Batteries

let file, set_file =
   let f = ref None in
   let m = RMutex.create () in
   let s f = RMutex.synchronize ~lock:m f in
   s (fun () -> !f), s (fun f' -> f := f')

let pipe_in, pipe_out = Unix.pipe ()

(*
let thread =
   let f () =
      let prev_file = 
      while true do
         let timeout =
            match file () with
            | None -> reset (); 1.0
            | Some f -> play_some_events f
         in
         ThreadUnix.timed_read "_" 0 1 timeout |> ignore
      done
   in
   Thread.create f () |> ignore
   *)

let notify () =
   Unix.write pipe_out "!" 0 1 |> ignore

let thread = ref (Thread.self ())

let play f =
   if Option.is_some (file ()) then
      failwith "already playing";
   set_file (Some f);
   print_endline "1";
   MidiFile.tracks f |> ignore;
   let do_play f =
      notify ();
      let time = ref 0 in
      Export.export_events f |> Enum.iter (fun (t, tr, cmd) ->
         let dtime = t - !time in
         Printf.printf "dt: %i\n%!" dtime;
         if dtime > 0 then (
            MidiIo.flush_output ();
            Thread.select [] [] [] (float_of_int dtime /. 600.0) |> ignore
         );
         time := t;
         cmd |> MidiCmd.to_string |> MidiIo.output
      );
   in
   thread := Thread.create do_play f;
   ThreadUnix.timed_read pipe_in "_" 0 1 100.0 |> ignore

(* vim: set ts=3 sw=3 tw=80 : *)
