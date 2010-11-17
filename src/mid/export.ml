open BatPervasives

let export_stream file =
   let enum_channel (chid, channel) =
      let enum_ctrl (ctrltype, ctrlmap) =
         CtrlMap.enum ctrlmap /@ (fun (time, v) -> time, chid, ctrltype, v)
      in
      channel#enum_ctrls /@ enum_ctrl
   in
   let ctrls = MiscUtils.enum_merge2 (file#enum_channels /@ enum_channel) in
   ignore ctrls;
   Stream.of_list [0, 0, MidiAsm.on 0 0 0];;

let export_file file filename =
   let channel = open_out_bin filename in
   close_out channel;;

(* vim: set ts=3 sw=3 tw=80 : *)
