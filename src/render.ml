open Segment
open Point
open Player
open Trigo
open Graphics
open Bsp
open Fsegment
open Options

let win_w = foi win_w
let win_h = foi win_h
let ceiling_h = foi ceiling_h
let floor_h = foi floor_h
let scalemap = ref scale

let calc_angle s =
  atan2 (s.yd -. s.yo) (s.xd -. s.xo)

let translation_rotation s p =
  let ns = rotation (translation s (new_point (-p.pos.x) (-p.pos.y))) (-p.pa) in
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
  { s with
    zuo = win_h /. 2. +. (ceiling_h -. p.yeux) *. p.d /. s.xo;
    zlo = win_h /. 2. +. (floor_h -. p.yeux)  *. p.d /. s.xo;
    zud = win_h /. 2. +. (ceiling_h -. p.yeux) *. p.d /. s.xd;
    zld = win_h /. 2. +. (floor_h -. p.yeux)  *. p.d /. s.xd;
    co = win_w /. 2. -. s.yo *. p.d /. s.xo;
    cd = win_w /. 2. -. s.yd *. p.d /. s.xd
  }

let projection_h s p =
  let xo = p.d
  and yo = win_w /. 2. -. s.yo *. p.d /. s.xo
  and xd = p.d
  and yd = win_w /. 2. -. s.yd *. p.d /. s.xd in
  if yo < 0. && yd < 0. ||  yo > win_w && yd > win_w then None
  else Some {s with xo; yo; xd; yd}
   
let draw2D x =
  let taille = !scalemap in
  let xo, yo, xd, yd = (iof x.xo) / taille,
    (iof x.yo) / taille,
    (iof x.xd) / taille,
    (iof x.yd) / taille
  in
  moveto ((xd + xo) / 2) ((yd + yo) / 2);
  draw_string x.id;
  draw_segments [|xo, yo, xd, yd|]

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
  let du = (s.zud -. s.zuo) /. (s.cd -. s.co) in
  let dl = (s.zld -. s.zlo) /. (s.cd -. s.co) in
  (*
     (* DEBUG *)
    Format.eprintf "(%.1f, %.1f, %.1f) (%.1f, %.1f, %.1f) (%.1f, %.1f)@."
    s.co s.zlo s.zuo s.cd s.zld s.zud du dl;
  *)
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
    end
(*
  (* DEBUG *)
  ;
  Format.eprintf "(%.1f, %.1f, %.1f) (%.1f, %.1f, %.1f)@,\
  ---------------------@."
  s.co s.zlo s.zuo s.cd s.zld s.zud
*)


let draw3D x =
  algo3D x;
  let co, zlo, zuo = iof x.co, iof x.zlo, iof x.zuo in
  let cd, zld, zud = iof x.cd, iof x.zld, iof x.zud in
  Format.eprintf "(%d, %d, %d) (%d, %d, %d)@." co zlo zuo cd zld zud;
  let (co, cd, zlo, zuo, zud, zld) =
    ((co)/ scale,
     (cd) / scale,
     (zlo) / scale,
     (zuo) / scale,
     (zud) / scale,
     (zld) / scale)
  in
  set_color (rgb 218 165 32);
  fill_poly ([|(co, zlo);
               (co, zuo);
               (cd, zud);
               (cd, zld)|]);
  set_color red;
    (*
     (* DEBUG *)
      Format.eprintf " %s (ci = %f, ce = %f)\n@." x.id x.ci x.ce;
    *)
  if x.ci > 0. && x.ce = 1. then
    draw_segments ([|(co, zlo, cd, zld);
                     (co, zuo, cd, zud);
                     (cd, zld, cd, zud)|])
  else if x.ce < 1. && x.ci = 0. then
    draw_segments ([|(co, zlo, cd, zld);
                     (co, zuo, cd, zud);
                     (co, zlo, co, zuo)|])
  else if x.ci > 0. && x.ce < 1. then
    draw_segments ([|(co, zlo, cd, zld);
                     (co, zuo, cd, zud)|])
  else
    draw_poly ([|(co, zlo);
                 (co, zuo);
                 (cd, zud);
                 (cd, zld)|])
(*
        (* DEBUG *)
  Printf.printf "(x.co = %f, zlo = %f)\n(x.co = %f, zuo = %f)"
  ^"\n(x.cd = %f, zud = %f)\n(x.cd = %f, zld = %f)\n"
  x.co x.zlo x.co x.zuo x.cd x.zud x.cd x.zld;
*)

let clip3D p r =
  let r = fsegment_of_seg r in
  let xo, yo = iof r.xo, iof r.yo in
  let xd, yd = iof r.xd, iof r.yd in
  Format.eprintf "(%d, %d) (%d, %d)@." xo yo xd yd;
  
  match translation_rotation r p with
  | None -> ()
  | Some tr ->
    match projection_h tr p with
    | None -> ()
    | Some ph ->
      let pv = projection_v tr p in
      draw3D pv
      
      
let draw_player2D taille p =
  set_color blue;
  let px = p.pos.x / taille in
  let py = p.pos.y / taille in
  fill_circle px py (10 / taille);
  set_color yellow;
  let i = ref (140 / taille) in
  let etal = !i / 10 in
  let fov = fov / 2 in
  while !i > 10 do
    draw_arc px py !i !i (p.pa - fov) (p.pa + fov);
    i := !i - etal;
  done;
  set_color blue
    
let win_w = iof win_w
let win_h = iof win_h
let ceiling_h = iof ceiling_h
  
let draw_player3D p =
  let yeux = iof p.yeux in
  (* head *)
  fill_circle (win_w / 2) (yeux + 100) 20;
  (* body *)
  draw_segments [|(win_w / 2,(yeux + 100) / 3,win_w / 2, yeux + 100)|];
  (* hands *)
  draw_segments ([|win_w / 2 - 50,((yeux + 100) / 3 + yeux + 100) / 2,
                    win_w / 2 + 50,((yeux + 100) / 3 + yeux + 100) / 2|]);
  (* legs *)
  draw_poly ([|(win_w / 2 - 20, 1);
               (win_w / 2, (yeux + 100) / 3);
               (win_w / 2 + 20, 1);
               (win_w / 2, (yeux + 100) / 3)|])
    
let draw_minimap map player =
  let taille = scale * 4 in
  set_line_width 2;
  (* contour *)
  set_color black;
  draw_rect 0 0 ((win_w + taille) / taille) ((win_h + taille) / taille);
  (* fond *)
  set_color cyan;
  fill_rect 0 0 (win_w / taille) (win_h / taille);
  set_line_width 1;
  if player.pos.x >= 0
    && player.pos.y >=0
    && player.pos.x <= win_w
    && player.pos.y <= win_h then
    draw_player2D taille player;
  set_color blue;
  set_line_width 2;
  scalemap := scale * 4;
  let f s = draw2D (fsegment_of_seg s) in
  parse f map player.pos;
  scalemap := scale

let display bsp player =
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
      set_color (rgb 102 51 0);
      fill_rect 0 0 win_w (win_h / 2);
      if get_time () = Day then set_color (rgb 51 204 255);
      if get_time () = Night then set_color (rgb 47 79 79);
      fill_rect 0 (win_h / 2) win_w win_h;
      set_color black;
      rev_parse (clip3D player) bsp player.pos;
      Format.eprintf "-----------------@.";
  
      if minimap then draw_minimap bsp player;
      if get_hov () = ceiling_h / 2 && get_perspective () = RPG then
        draw_player3D player
      else ()
    end
