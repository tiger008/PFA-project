open Fsegment
open Options
open Player
open Point
open Segment

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

let win_w = float win_w
let win_h = float win_h
let ceiling_h = float ceiling_h
let floor_h = float floor_h

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

let calc_angle s =
  atan2 (s.yd -. s.yo) (s.xd -. s.xo)

let translation_rotation s p =
  let pp = new_point (-p.pos.x) (-p.pos.y) in
  let ns = rotation (translation s pp) (-p.pa) in
  let ns = {ns with angle = calc_angle ns} in
  if (ns.xo < 1. && ns.xd < 1.) then None
  else if ns.xo < 1. then
    Some {ns with
           xo = 1.;
           yo = (ns.yo +. (1. -. ns.xo) *. (tan ns.angle))}
  else if ns.xd < 1. then
    Some {ns with
           xd = 1.;
           yd = (ns.yd +. (1. -. ns.xd) *. (tan ns.angle))}
  else Some ns
            
let translation_rotation_inverse s p =
  let ns = translation (rotation s p.pa) p.pos in
  {ns with angle = calc_angle ns}

let projection_h s p =
  let xo = p.d
  and yo = win_w /. 2. -. s.yo *. p.d /. s.xo
  and xd = p.d
  and yd = win_w /. 2. -. s.yd *. p.d /. s.xd in
  if yo < 0. && yd < 0. ||  yo > win_w && yd > win_w then None
  else Some {s with xo; yo; xd; yd}
            
let projection_v s p =
  let yeux = foi (get_hov ()) in
  (* DEBUG *)
  (* Format.eprintf "(%.1f, %.1f, %.1f) (%.1f, %.1f, %.1f)@." *)
  (*                s.co s.zlo s.zuo s.cd s.zld s.zud; *)
  let s = { s with
            zuo = win_h /. 2. +. (ceiling_h -. yeux) *. p.d /. s.xo;
            zlo = win_h /. 2. +. (floor_h -. yeux)  *. p.d /. s.xo;
            zud = win_h /. 2. +. (ceiling_h -. yeux) *. p.d /. s.xd;
            zld = win_h /. 2. +. (floor_h -. yeux)  *. p.d /. s.xd;
            co = win_w /. 2. -. s.yo *. p.d /. s.xo;
            cd = win_w /. 2. -. s.yd *. p.d /. s.xd
          }
  in
  (* DEBUG *)
  (* Format.eprintf "(%.1f, %.1f, %.1f) (%.1f, %.1f, %.1f)@." *)
  (*                s.co s.zlo s.zuo s.cd s.zld s.zud; *)
  s

let between x lb ub =
  x >= lb && x <= ub
                    
let lstocl s1 s2 =
  let bo = get_position s2.porig s1 in
  let bd = get_position s2.pdest s1 in
  let xo1, yo1 = s1.porig.x, s1.porig.y
  and xd1, yd1 = s1.pdest.x, s1.pdest.y
  and xo2, yo2 = s2.porig.x, s2.porig.y
  and xd2, yd2 = s2.pdest.x, s2.pdest.y
  in
  match bo, bd with
  | C, _ when between xo2 xo1 xd1 && between yo2 yo1 yd1 -> true
  | _, C when between xd2 xo1 xd1 && between yd2 yo1 yd1 -> true
  | R, L -> true
  | L, R -> true
  | _ -> false

let intersect s1 s2 = lstocl s1 s2 && lstocl s2 s1
