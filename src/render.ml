open Segment
open Point
open Player
open Trigo
open Graphics
open Bsp
open Fsegment
open Options
open Draw

let win_w = foi win_w
let win_h = foi win_h
let ceiling_h = foi ceiling_h
let floor_h = foi floor_h
let scalemap = ref scale

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

let projection_v s p =
  let yeux = foi (get_hov ()) in
  (* DEBUG *)
  Format.eprintf "(%.1f, %.1f, %.1f) (%.1f, %.1f, %.1f)@."
                 s.co s.zlo s.zuo s.cd s.zld s.zud;
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

let projection_h s p =
  let xo = p.d
  and yo = win_w /. 2. -. s.yo *. p.d /. s.xo
  and xd = p.d
  and yd = win_w /. 2. -. s.yd *. p.d /. s.xd in
  if yo < 0. && yd < 0. ||  yo > win_w && yd > win_w then None
  else Some {s with xo; yo; xd; yd}

let clip2D r p =
  let r = fsegment_of_seg r in
  match translation_rotation r p with
  | None -> ()
  | Some tr ->
     match projection_h tr p with
     | None -> ()
     | Some ph -> draw2D (translation_rotation_inverse tr p)

let algo3D s =
  let ls = win_w in
  let ymax = 20000. in
  let ymin = -.ymax in
  let du = (s.zud -. s.zuo) /. (s.cd -. s.co) in
  let dl = (s.zld -. s.zlo) /. (s.cd -. s.co) in
  
  (*  (\* DEBUG *\) *)
  (* Format.eprintf "(%.1f, %.1f, %.1f) (%.1f, %.1f, %.1f) (%.1f, %.1f)@." *)
  (* s.co s.zlo s.zuo s.cd s.zld s.zud du dl; *)
  if s.co < 0. then
    begin
      s.zuo <- s.zuo -. (s.co *. du);
      s.zlo <- s.zlo -. (s.co *. dl);
      s.co <- 0.;
    end
  else if s.co > ls then
    begin
      s.zuo <- s.zuo -. ((s.co -. ls) *. du);
      s.zlo <- s.zlo -. ((s.co -. ls) *. dl);
      s.co <- ls;
    end;
  if s.cd < 0. then
    begin
      s.zud <- s.zud -. (s.cd *. du);
      s.zld <- s.zld -. (s.cd *. dl);
      s.cd <- 0.;
    end
  else if s.cd > ls then
    begin
      s.zud <- s.zud -. ((s.cd -. ls) *. du);
      s.zld <- s.zld -. ((s.cd -. ls) *. dl);
      s.cd <- ls;
    end;
  if s.zlo < ymin then s.zlo <- ymin;
  if s.zuo > ymax then s.zuo <- ymax;
  if s.zld < ymin then s.zld <- ymin;
  if s.zud > ymax then s.zud <- ymax;
  if s.zlo = ymin && s.zuo = ymax && s.co < s.cd then s.co <- 0.
  else if s.zlo = ymin && s.zuo = ymax && s.co > s.cd then s.co <- 800.;
  if s.zld = ymin && s.zud = ymax && s.cd < s.co then s.cd <- 0.
  else if s.zld = ymin && s.zud = ymax && s.cd > s.co then s.cd <- 800.
  (* (\* DEBUG *\) *)
  (* ; *)
  (* Format.eprintf "(%.1f, %.1f, %.1f) (%.1f, %.1f, %.1f)@,\ *)
  (*                 ---------------------@." *)
  (*                s.co s.zlo s.zuo s.cd s.zld s.zud *)
                 
let clip3D p r =
  let r = fsegment_of_seg r in
  let xo, yo = iof r.xo, iof r.yo in
  let xd, yd = iof r.xd, iof r.yd in
  (* Format.eprintf "(%d, %d) (%d, %d)@." xo yo xd yd; *)
  match translation_rotation r p with
  | None -> ()
  | Some tr ->
     match projection_h tr p with
     | None -> ()
     | Some ph ->
        let pv = projection_v tr p in
        algo3D pv;
        draw3D pv

let display bsp player sun moon =
  let win_w = iof win_w in
  let win_h = truncate win_h in
  let ceiling_h = iof ceiling_h in
  if get_mode () = TwoD then
    begin
      (* fond *)
      set_color cyan;
      fill_rect 0 0 win_w win_h;
      set_color black;
      draw_player2D scale player;
      let f s = clip2D s player in
      rev_parse f bsp player.pos;
    end
  else
    begin
      let landsky_inter = win_h / 2 + get_hov () in
      Format.eprintf "%d landsky_inter\n@." landsky_inter;
      if get_time () = Day then
        begin
          set_color (rgb 51 204 255);
          if landsky_inter < 0 then
            fill_rect 0 0 win_w win_h
          else
            fill_rect 0 landsky_inter win_w win_h;
          draw_sun sun player landsky_inter;
        end
      else if get_time () = Night then
        begin
          set_color (rgb 47 79 79);
          fill_rect 0 landsky_inter win_w win_h;
          draw_moon moon player landsky_inter;
        end;
      set_color (rgb 102 51 0);
      if landsky_inter > 0 then
        fill_rect 0 0 win_w landsky_inter;
      set_color black;
      rev_parse (clip3D player) bsp player.pos;
      Format.eprintf "-----------------@.";
      if minimap then draw_minimap bsp player;
      if get_hov () = ceiling_h / 2 && get_perspective () = RPG then
        draw_player3D player
      else ()
    end
