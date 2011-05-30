open Cairo

(* X axis constants *)
let left_margin, right_margin = 1., 1.

(* Y axis constants *)
let track_height = 9.0
let top_margin, bottom_margin = 1., 1.

let font = ("monospace", FONT_SLANT_NORMAL, FONT_WEIGHT_NORMAL, 14.)

(* Measure numbers *)
let measure_font = ("monospace", FONT_SLANT_NORMAL, FONT_WEIGHT_NORMAL, 12.)
let measure_color = "#f00000"
let measure_x = TabX.chars 0.5 and measure_y = 2.5

let foreground = "black"
let background = "#ffffee"
let string_color = "#e8e8e8"
let measurebar_color = "black"

(* vim: set ts=3 sw=3 tw=80 : *)
