class file =
   object (self)
      val mutable filename_ = ""

      method filename = filename_
      method set_filename _filename = filename_ <- _filename
      method export fn =
         Export.export self fn;
         self#set_filename fn;
   end;;

(* vim: set ts=3 sw=3 tw=80 : *)