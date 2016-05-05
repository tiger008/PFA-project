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
  | Left -> p.pa <- (p.pa + 15) mod 360
  | Right -> p.pa <- (p.pa - 15) mod 360

type mv = MFwd | MBwd | MLeft | MRight

let iof n = int_of_float n

let foi n = float_of_int n

let move d p bsp =
  Printf.printf "position x : %d\n" p.pos.x;
  Printf.printf "position y : %d\n" p.pos.y;
  let npa = p.pa-90 in
  match d with
  | MFwd -> let np = new_point (iof (ceil ((foi (p.pos.x)) -. 5. *. (dsin (npa))))) (iof (ceil ((foi (p.pos.y)) +. 5. *. (dcos (npa))))) in
            if not (detect_collision np bsp) then
              p.pos <- np
            else ()
  | MBwd -> let np = new_point (iof (floor ((foi (p.pos.x)) +. 5. *. (dsin (npa))))) (iof (floor ((foi (p.pos.y)) -. 5. *. (dcos (npa))))) in
            if not (detect_collision np bsp) then
              p.pos <- np
            else ()
  | MLeft -> let np = new_point (iof (ceil ((foi (p.pos.x)) -. 5. *. (dcos (npa))))) (iof (ceil ((foi (p.pos.y)) -. 5. *. (dsin (npa))))) in
             if not (detect_collision np bsp) then
               p.pos <- np
             else ()
  | MRight -> let np = new_point (iof (floor ((foi (p.pos.x)) +. 5. *. (dcos (npa))))) (iof (floor ((foi (p.pos.y)) +. 5. *. (dsin (npa))))) in
              if not (detect_collision np bsp) then
                p.pos <- np
              else ()
