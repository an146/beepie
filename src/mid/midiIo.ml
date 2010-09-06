type device_type = InputDevice | OutputDevice;;
type device_id = string;;
type device = { id: device_id; name: string; };;

external init: unit -> unit = "io_init"
external fini: unit -> unit = "io_fini"
external enum_devices: device_type -> device list = "io_enum_devices";;
external get_device: device_type -> device_id = "io_get_device";;
external set_device: device_type -> device_id -> unit = "io_set_device";;
external output: string -> unit = "io_output";;
external flush_output: unit -> unit = "io_flush_output";;

let enum_output_devices () = enum_devices OutputDevice;;
let get_output_device () = get_device OutputDevice;;
let set_output_device = set_device OutputDevice;;

let compose_string a =
   let len = Array.length a in
   let s = String.create len in
   for i = 0 to len - 1 do
      s.[i] <- char_of_int a.(i)
   done;
   s;;

let output_a a = output (compose_string a)

let output_note channel pitch length =
   output_a [| 0x90 + channel; pitch; 64 |];
   flush_output ();
   ignore (Thread.select [] [] [] length);
   output_a [| 0x80 + channel; pitch; 64 |];
   flush_output ();;

let set_program channel program =
   output_a [| 0xC0 + channel; program |];;

(* vim: set ts=3 sw=3 tw=80 : *)
