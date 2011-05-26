type t = {
   chars  : float;
   spaces : float;
}

let make c s = {chars = c; spaces = s}
let makei c s = {chars = float c; spaces = float s}

let zero = make 0. 0.
let chars c = make c 0.
let charsi c = makei c 0
let spaces s = make 0. s
let spacesi s = makei 0 s
let text str = charsi (String.length str)
let char = charsi 1
let space = spacesi 1
let space_size x cw = (cw -. x.chars) /. x.spaces
let size x ssize = x.chars +. x.spaces *. ssize

let op op a b =
   {
      chars = op a.chars b.chars;
      spaces = op a.spaces b.spaces;
   }

let add a b = op (+.) a b
let max a b = op max a b

let printer x =
   Printf.sprintf "(%f:%f)" x.chars x.spaces

module Infix = struct
   let (+:) = add
end

(* vim: set ts=3 sw=3 tw=80 : *)
