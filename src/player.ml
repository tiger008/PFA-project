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

let iof n = int_of_float n

let foi n = float_of_int n

let move d p bsp =
    let dx, dy =
        match d with
        | MFwd -> 0., -1.
        | MBwd -> 0., 1.
        | MLeft -> -1., 0.
        | MRight -> 1., 0. in
    let np = Point.new_point (iof (((foi p.pos.x) +. dx) *. cos (foi p.pa))) (iof (((foi p.pos.y) +. dy) *. sin (foi p.pa))) in
    if not (detect_collision np bsp) then
        p.pos <- np
    else ()
