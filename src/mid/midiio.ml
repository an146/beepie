type device_type = InputDevice | OutputDevice;;

external init: unit -> unit = "io_init"
external fini: unit -> unit = "io_fini"
external enum_devices: device_type -> string list = "io_enum_devices";;
external set_device: device_type -> string -> unit = "io_set_device";;
external output: string -> unit = "io_output";;
external flush_output: unit -> unit = "io_flush_output";;

let enum_output_devices () = enum_devices OutputDevice;;
let set_output_device = set_device OutputDevice;;

