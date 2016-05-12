open Point
open Segment

type t = {
    id : string;
    xo : float;
    yo : float;
    xd : float;
    yd : float;
    ci : float;
    ce : float;
    angle : float;
    mutable zuo : float;
    mutable zlo : float;
    mutable zud : float;
    mutable zld : float;
    mutable co : float;
    mutable cd : float
}

let new_fsegment xo yo xd yd = {
  id = get_id ();
  xo = xo;
  yo = yo;
  xd = xd;
  yd = yd;
  ci = 0.;
  ce = 1.;
  angle = atan2 (yd -. yo) (xd -. xo);
  zuo = 0.;
  zlo = 0.;
  zud = 0.;
  zld = 0.;
  co = 0.;
  cd = 0.
}

let fsegment_of_seg s =
  let (xo, yo, xd, yd) = get_coordonnees s in
  let fs = new_fsegment xo yo xd yd in
  {fs with id = s.id; ci = s.ci; ce = s.ce}


