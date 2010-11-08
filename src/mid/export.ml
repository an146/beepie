let export_stream file =
   Stream.of_list [0, 0, MidiAsm.on 0 0 0];;

let export_file file filename =
   let channel = open_out_bin filename in
   close_out channel;;

(* vim: set ts=3 sw=3 tw=80 : *)
