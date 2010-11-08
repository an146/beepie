module type OrderedType =
   sig
      type t
      val compare : t -> t -> int
   end

module Make(T : OrderedType) = struct
   type elt = T.t
   type t = elt list

   let empty = []
   let add element tree = element :: tree
   let height tree = List.length tree
end

(* vim: set ts=3 sw=3 tw=80 : *)
