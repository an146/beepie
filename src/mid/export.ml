let export file filename =
   let channel = open_out_bin filename in
   close_out channel;

(* vim: set ts=3 sw=3 tw=80 : *)
