open Options
open Physic
open Point
open Trigo
open Segment

type t = {
    mutable pos : Point.t;
    mutable pa : int;
}

let new_player pos pa = { pos; pa }

type dir = Left | Right

let rotate d p =
  let a = Printf.printf "%d\n" p.pa in
  match d with
  | Left -> p.pa <- (p.pa + 5) mod 360
  | Right -> p.pa <- (p.pa - 5) mod 360

type mv = MFwd | MBwd | MLeft | MRight

let iof n = int_of_float n

let foi n = float_of_int n

let move d p bsp =
    let dx, dy =
        match d with
        | MFwd -> 0., 2.
        | MBwd -> 0., -2.
        | MLeft -> -2., 0.
        | MRight -> 2., 0. in
    let np = new_point (iof (foi (p.pos.x) +. 2. *. (dcos p.pa))) (iof (foi (p.pos.y) +. 2. *. (dsin p.pa))) in
    if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
        p.pos <- np
    else ()
