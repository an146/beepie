open Cairo

module Style = struct
   (* X axis constants *)
   let left_margin, right_margin = 1., 1.

   (* Y axis constants *)
   let track_height = 9.0
   let top_margin, bottom_margin = 1., 1.

   let font =         ("monospace", FONT_SLANT_NORMAL, FONT_WEIGHT_NORMAL, 14.)

   (* Measure numbers *)
   let measure_font = ("monospace", FONT_SLANT_NORMAL, FONT_WEIGHT_NORMAL, 12.)
   let measure_color = "#f00000"
   let measure_x = TabX.chars 0.5 and measure_y = 2.5

   let foreground = "black"
   let background = "#ffffee"
   let string_color = "#e8e8e8"
   let measurebar_color = "black"
end

let select_font c (name, slant, weight, size) =
   select_font_face c name slant weight;
   set_font_size c size

let get_font_extents font =
   let s = image_surface_create FORMAT_A1 ~width:1 ~height:1 in
   let c = create s in
   select_font c font;
   font_extents c

let set_source_color c color =
   Cairo_lablgtk.set_source_color c (GDraw.color (`NAME color))

(* vim: set ts=3 sw=3 tw=80 : *)
