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
let (xo, yo, xd, yd) = get_coordonnees s in
let fs = new_fsegment xo yo xd yd in
{fs with id = s.id}


