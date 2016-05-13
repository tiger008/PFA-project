open Fsegment
open Options
open Player
open Bsp
open Graphics
open Point
open Sun
open Moon

let scalemap = ref scale

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

let draw3D x =
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

let draw_sun sun player landsky_inter =
  set_color yellow;
  if player.pa = 90 then
    begin
      sun.spos <- 0;
    end
  else if player.pa = 90+fov then
    begin
      sun.spos <- win_w
    end;
  if sun.ssize > 0 then
    fill_circle (sun.spos) (landsky_inter+win_h/4) (sun.ssize)

let draw_moon moon player landsky_inter =
  set_color (rgb 192 192 192);
  if player.pa = 90 then
    begin
      moon.mpos <- win_w
    end;
  if moon.msize > 0 then
    fill_circle (moon.mpos) (landsky_inter+win_h/4) (moon.msize)
