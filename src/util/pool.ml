type 'a t = 'a Stack.t * (unit -> 'a)

let get (s, f) =
   try Stack.pop s
   with _ -> f ()

let put v (s, _) =
   Stack.push v s

let create f n =
   let p = (Stack.create (), f) in
   for i = 1 to n do
      put (get p) p
   done;
   p

(* vim: set ts=3 sw=3 tw=80 : *)
