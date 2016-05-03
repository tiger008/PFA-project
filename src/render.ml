open Segment
open Point
open Player
open Trigo

let iof n = int_of_float n

let foi n = float_of_int n

let tr s p =
    {s with
    porig = new_point (iof ((foi (s.porig.x - p.pos.x)) *. (dcos (-p.pa)) -. (foi (s.porig.y - p.pos.y)) *. (dsin (-p.pa)))) (iof ((foi (s.porig.y - p.pos.y)) *. (dcos (-p.pa)) -. (foi (s.porig.x - p.pos.x)) *. (dsin (-p.pa))));
    pdest = new_point (iof ((foi (s.pdest.x - p.pos.x)) *. (dcos (-p.pa)) -. (foi (s.pdest.y - p.pos.y)) *. (dsin (-p.pa)))) (iof ((foi (s.pdest.y - p.pos.y)) *. (dcos (-p.pa)) -. (foi (s.pdest.x - p.pos.x)) *. (dsin (-p.pa))))
    }

let angle s =
    atan (foi (s.pdest.y - s.porig.y)) /. (foi (s.pdest.x - s.porig.x))

let rec clip ls acc =
  match ls with
  | [] -> acc
  | p::s ->
     if p.porig.x < 1 && p.pdest.x < 1 then
       clip s acc
     else if p.porig.x < 1 then
       let seg = {p with porig = new_point 1 (iof (foi (p.porig.y + (1 - p.porig.x)) *. dtan (iof (angle p)))); pdest = new_point p.pdest.x p.pdest.y} in
       clip s (seg::acc)
     else if p.porig.y < 1 then
       let seg = {p with porig = new_point p.porig.x p.porig.y; pdest = new_point 1 (iof (foi (p.pdest.y + (1 - p.pdest.x)) *. dtan (iof (angle p))))} in
       clip s (seg::acc)
     else clip s (p::acc)

(*let rec array_segments bsp acc =
  match bsp with
  | E -> acc
  | N(s,sl,sr) -> 
      let draw_player*)

let display bsp p = failwith "TODO"
