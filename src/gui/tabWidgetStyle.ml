module C = Cairo

module Style = struct
   (* X axis constants *)
   let left_margin, right_margin = 1., 1.

   (* Y axis constants *)
   let track_height = 8.0
   let top_margin, bottom_margin = 1., 1.

   let font = ("monospace", C.FONT_SLANT_NORMAL, C.FONT_WEIGHT_NORMAL, 14.)
end

let select_font c (name, slant, weight, size) =
   C.select_font_face c name slant weight;
   C.set_font_size c size

(* vim: set ts=3 sw=3 tw=80 : *)
