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
  match d with
  | Left -> p.pa <- (p.pa + 15) mod 360
  | Right -> p.pa <- (p.pa - 15) mod 360

type mv = MFwd | MBwd | MLeft | MRight

let segtoarray s =
  Array.of_list [(s.porig.x + iof (s.ci *. (foi s.lx)), s.porig.y + iof (s.ci *. (foi s.ly)), s.pdest.x + iof (s.ce *. (foi s.lx)), s.pdest.y + iof (s.ce *. (foi s.ly)))]

let move d p bsp =
  let npa = p.pa-90 in
  match d with
  | MFwd -> let np = new_point (iof (ceil ((foi (p.pos.x)) -. 5. *. (dsin (npa))))) (iof (ceil ((foi (p.pos.y)) +. 5. *. (dcos (npa))))) in
            if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
              p.pos <- np
            else ()
  | MBwd -> let np = new_point (iof (floor ((foi (p.pos.x)) +. 5. *. (dsin (npa))))) (iof (floor ((foi (p.pos.y)) -. 5. *. (dcos (npa))))) in
            if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
              p.pos <- np
            else ()
  | MLeft -> let np = new_point (iof (ceil ((foi (p.pos.x)) -. 5. *. (dcos (npa))))) (iof (ceil ((foi (p.pos.y)) -. 5. *. (dsin (npa))))) in
            if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
               p.pos <- np
             else ()
  | MRight -> let np = new_point (iof (floor ((foi (p.pos.x)) +. 5. *. (dcos (npa))))) (iof (floor ((foi (p.pos.y)) +. 5. *. (dsin (npa))))) in
            if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
                p.pos <- np
              else ()
