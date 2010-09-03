let import filename =
   let file = new Midifile.file in
   let channel = open_in_bin filename in
   close_in channel;
   file#set_filename filename;
   file;;

(* vim: set ts=3 sw=3 tw=80 : *)
