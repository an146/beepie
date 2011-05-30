open Cairo

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
