open Options
open Physic
open Point
open Trigo
open Segment

type t = {
    mutable pos : Point.t;
    mutable pa : int;
    d : float;
    yeux : float
}

let new_player pos pa ls fov  = {
    pos;
    pa;
    d = ((foi ls) /. 2.) /. dtan (fov / 2);
    yeux = foi hov
}

type dir = Left | Right

let rotate d p =
  match d with
  | Left -> p.pa <- (p.pa + 5) mod 360
  | Right -> p.pa <- (p.pa - 5) mod 360

type mv = MFwd | MBwd | MLeft | MRight

let move d p bsp =
  let npa = p.pa-90 in
  match d with
  | MFwd -> let np = new_point (iof (ceil ((foi (p.pos.x))
                                    -. step_dist *. (dsin (npa)))))
                               (iof (ceil ((foi (p.pos.y))
                                    +. step_dist *. (dcos (npa)))))
            in
            if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
              p.pos <- np
            else ()
  | MBwd -> let np = new_point (iof (floor ((foi (p.pos.x))
                                    +. step_dist *. (dsin (npa)))))
                               (iof (floor ((foi (p.pos.y))
                                    -. step_dist *. (dcos (npa)))))
            in
            if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
              p.pos <- np
            else ()
  | MLeft -> let np = new_point (iof (ceil ((foi (p.pos.x))
                                    -. step_dist *. (dcos (npa)))))
                                (iof (ceil ((foi (p.pos.y))
                                    -. step_dist *. (dsin (npa)))))
             in
             if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
               p.pos <- np
             else ()
  | MRight -> let np = new_point (iof (floor ((foi (p.pos.x))
                                    +. step_dist *. (dcos (npa)))))
                                 (iof (floor ((foi (p.pos.y))
                                    +. step_dist *. (dsin (npa)))))
              in
              if not (detect_real (new_segment p.pos.x p.pos.y np.x np.y) bsp) then
                p.pos <- np
              else ()
