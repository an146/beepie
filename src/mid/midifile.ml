class track =
   object (self)
   end

class file (_division : int) =
   object (self)
      val division_ = _division
      val mutable filename_ = ""
      val mutable tracks_ : track list = []

      method division = division_
      method filename = filename_
      method set_filename _filename = filename_ <- _filename
      method export fn =
         Export.export self fn;
         self#set_filename fn
      method add_track () =
         let ret = new track in
         tracks_ <- tracks_ @ [ret];
         ret
   end;;

(* vim: set ts=3 sw=3 tw=80 : *)
