open Options
open Physic
open Point
open Trigo

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
        | MFwd -> 0, 1
        | MBwd -> 0, -1
        | MLeft -> -1, 0
        | MRight -> 1, 0 in
    let np = new_point (p.pos.x + dx) (p.pos.y + dy) in
        (* 3D new_point (iof (foi (p.pos.x + dx) *. (dcos p.pa))) (iof ((foi (p.pos.y + dy)) *. (dsin p.pa))) in*)
    if not (detect_collision np bsp) then
        p.pos <- np
    else ()
