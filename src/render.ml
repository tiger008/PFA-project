open Segment
open Point
open Player
open Trigo
open Graphics
open Bsp
open Fsegment
open Options


let scalemap = ref scale

let calc_angle s =
  atan2 (s.yd -. s.yo) (s.xd -. s.xo)

let translation_rotation s p =
  let ns = rotation (translation s (new_point (-p.pos.x) (-p.pos.y))) (-p.pa) in
  {ns with angle = calc_angle ns}

let translation_rotation_inverse s p =
  let ns = translation (rotation s p.pa) p.pos in
  {ns with angle = calc_angle ns}

let projection_h s p =
    {s with
    xo = p.d;
    yo = (foi win_w) /. 2. -. s.yo *. p.d /. s.xo;
    xd = p.d;
    yd = (foi win_w) /. 2. -. s.yd *. p.d /. s.xd
    }

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
      let tr = translation_rotation r p in
      let ph = projection_h tr p in
      (* Segment complètement masqués *)
      if (tr.xo < 1. && tr.xd < 1.)
        || (ph.yo < 0. && ph.yd < 0.)
        || (ph.yo > foi win_w && ph.yd > foi win_w) then
        ()
      else if tr.xo < 1. then
        begin
        (* Calcul des nouvelles coordonnées *)
        let tr = {tr with
          xo = 1.;
          yo = (tr.yo +. (1. -. tr.xo) *. (tan tr.angle))} in
        (* On projette et determine si le segment est invisible *)
        let ph = projection_h tr p in
        if (ph.yo < 0. && ph.yd < 0.)
          || (ph.yo > foi win_w && ph.yd > foi win_w) then
          ()
        else
          let r = translation_rotation_inverse tr p in
          draw2D r
        end
      else if tr.xd < 1. then
        begin
        (* Calcul des nouvelles coordonnées *)
        let tr = {tr with
          xd = 1.;
          yd = (tr.yd +. (1. -. tr.xd) *. (tan tr.angle))} in
        (* On projette et determine si le segment est invisible *)
        let ph = projection_h tr p in
        if (ph.yo < 0. && ph.yd < 0.)
          || (ph.yo > foi win_w && ph.yd > foi win_w) then
          ()
        else
          let r = translation_rotation_inverse tr p in
          draw2D r
        end
      else draw2D r

let algo3D s =
  let ls = foi win_w in
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


let projection_v s p =
  { s with
    zuo = (foi (win_h / 2)) +. (((foi ceiling_h) -. p.yeux) *. p.d /. s.xo);
    zlo = (foi (win_h / 2)) +. (((foi floor_h) -. p.yeux)  *. p.d /. s.xo);
    zud = (foi (win_h / 2)) +. (((foi ceiling_h) -. p.yeux) *. p.d /. s.xd);
    zld = (foi (win_h / 2)) +. (((foi floor_h) -. p.yeux)  *. p.d /. s.xd);
    co = (foi win_w) /. 2. -. s.yo *. p.d /. s.xo;
    cd = (foi win_w) /. 2. -. s.yd *. p.d /. s.xd
  }

let draw3D x =
  let taille = scale in
    algo3D x;
    let (co, cd, zlo, zuo, zud, zld) =
      ((iof x.co) / taille,
       (iof x.cd) / taille,
       (iof x.zlo) / taille,
       (iof x.zuo) / taille,
       (iof x.zud) / taille,
       (iof x.zld) / taille)
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

let clip3D r p =
     let r = fsegment_of_seg r in
     let tr = translation_rotation r p in
     let pv = projection_v tr p in
     let ph = projection_h pv p in
    (* Segment complètement masqués *)
    if (tr.xo < 1. && tr.xd < 1.)
      || (ph.yo < 0. && ph.yd < 0.)
      || (ph.yo > foi win_w && ph.yd > foi win_w) then
      ()
    else if tr.xo < 1. then
      begin
      (* Calcul des nouvelles coordonnées *)
      let tr = {tr with
        xo = 1.;
        yo = (tr.yo +. (1. -. tr.xo) *. (tan tr.angle))} in
      (* On projette et determine si le segment est invisible *)
      let ph = projection_h tr p in
      if (ph.yo < 0. && ph.yd < 0.)
        || (ph.yo > foi win_w && ph.yd > foi win_w) then
        ()
      else
        let r = projection_h (projection_v tr p) p in
        draw3D r
      end
    else if tr.xd < 1. then
      begin
      (* Calcul des nouvelles coordonnées *)
      let tr = {tr with
        xd = 1.;
        yd = (tr.yd +. (1. -. tr.xd) *. (tan tr.angle))} in
      (* On projette et determine si le segment est invisible *)
      let ph = projection_h tr p in
      if (ph.yo < 0. && ph.yd < 0.)
        || (ph.yo > foi win_w && ph.yd > foi win_w) then
        ()
      else
        let r = projection_h (projection_v tr p) p in
        draw3D r
      end
    else draw3D ph

let rec bsp_to_list bsp p =
  let acc = ref [] in
  let f s = acc := s::!acc in
  rev_parse f bsp p;
  !acc

let draw_player taille p =
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
    draw_player taille player;
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
      draw_player scale player;
      let f s = clip2D s player in
      rev_parse f bsp player.pos;
    end
  else
    begin
      set_color (rgb 102 51 0);
      fill_rect 0 0 win_w (win_h / 2);
      set_color (rgb 51 204 255);
      fill_rect 0 (win_h / 2) win_w win_h;
      set_color black;
      let f s = clip3D s player in
      rev_parse f bsp player.pos;
      if minimap then
        begin
          draw_minimap bsp player
        end;
    end
