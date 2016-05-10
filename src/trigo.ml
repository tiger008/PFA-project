open Point
open Segment
open Fsegment

let pi = 4. *. atan 1.

let pidiv = pi /. 180.
let ipidiv = 180. /. pi

let d_to_rad a = float a *. pidiv
let r_to_deg a = a *. ipidiv

let rtan a = tan a
let dtan a = tan (d_to_rad a)

let dcos a = cos (d_to_rad a)

let dacos c = r_to_deg (acos c)

let dsin a = sin (d_to_rad a)

let rotation s a =
    {s with
    xo = (s.xo *. (dcos a) -. s.yo *. (dsin a));
  yo = (s.xo *. (dsin a) +. s.yo *. (dcos a));
  xd = (s.xd *. (dcos a) -. s.yd *. (dsin a));
  yd = (s.xd *. (dsin a) +. s.yd *. (dcos a))
    }

let translation s t =
    {s with
    xo = (s.xo +. (foi t.x));
    yo = (s.yo +. (foi t.y));
    xd = (s.xd +. (foi t.x));
    yd = (s.yd +. (foi t.y))
    }
