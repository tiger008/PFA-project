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
    atan (foi (s.pdest.y - s.porig.y))/.(foi (s.pdest.x - s.porig.x))

let display bsp p = 
