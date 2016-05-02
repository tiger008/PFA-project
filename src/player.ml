open Options
open Physic
open Point
       
type t = {
  mutable pos : Point.t;
  mutable pa : int;
}

let new_player pos pa = { pos; pa }

type dir = Left | Right

let rotate d p =
  match d with
  | Left -> p.pa <- (p.pa + 1) mod 360
  | Right -> p.pa <- (p.pa - 1) mod 360

type mv = MFwd | MBwd | MLeft | MRight

let move d p bsp =
  let dx, dy =
  match d with
  | MFwd -> 0., -1.
  | MBwd -> 0., 1.
  | MLeft -> -1., 0.
  | MRight -> 1., 0. in
  let np = Point.new_point (int_of_float (((float_of_int p.pos.x) +. dx) *. cos (float_of_int p.pa))) (int_of_float (((float_of_int p.pos.y) +. dy) *. sin (float_of_int p.pa))) in
  if not (detect_collision np bsp) then
    p.pos <- np
  else ()
