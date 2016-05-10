open Point
open Segment

type t = {
    id : string;
    xo : float;
    yo : float;
    xd : float;
    yd : float;
    angle : float
}

let new_fsegment xo yo xd yd = {
   id = get_id ();
   xo = xo;
   yo = yo;
   xd = xd;
   yd = yd;
   angle = atan2 (yd -. yo) (xd -. xo)
}

let fsegment_of_seg s =
  new_fsegment ((foi s.porig.x) +. (foi s.lx) *. s.ci)
  ((foi s.porig.y) +. (foi s.ly) *. s.ci)
  ((foi s.porig.x) +. (foi s.lx) *. s.ce)
  ((foi s.porig.y) +.(foi s.ly) *. s.ce)


