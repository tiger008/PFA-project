open Bsp
open Point
open Segment
open Trigo

let detect_collision s bsp =
  let rec rdc bsp =
    match bsp with
    | E -> false
    | N(r,ag, ad) ->
       let xo = get_position s.porig r in
       let xd = get_position s.pdest r in
       let c  = intersect s r in
       if c then true
       else
         match xo, xd with
         | L, L | L, C | C, L -> rdc ag (* o---d|| ou o---|d| ou d---|o| *)
         | R, R | R, C | C, R -> rdc ad (* ||o---d ou |d|---o ou |o|---d *)
         | _ -> rdc ag && rdc ad
  in rdc bsp
