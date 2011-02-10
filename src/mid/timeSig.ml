type t = int * int

let make n d = n, d

let create n d =
   let rec lg ?(acc = 0) n =
      match n, n mod 2 with
      | 1, _ ->
            acc
      | even, 0 ->
            lg ~acc:(acc + 1) (even / 2)
      | _ ->
            failwith "TimeSig.create: denominator must be a power of 2"
   in
   n, lg d

let numer (n, _) = n
let denom (_, d) = d

(* vim: set ts=3 sw=3 tw=80 : *)
