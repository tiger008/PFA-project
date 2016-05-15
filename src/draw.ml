open Bsp
open Fsegment
open Graphics
open Moon
open Options
open Physic
open Player
open Point
open Sun
open Trigo
       
let scalemap = ref scale                 
                   
let draw2D x p =
  let taille = !scalemap in
  let xo, yo, xd, yd = ((iof x.xo) * win_w / 800) / taille,
                       ((iof x.yo) * win_h / 800) / taille,
                       ((iof x.xd) * win_w / 800) / taille,
                       ((iof x.yd) * win_h / 800) / taille
  in
  (* affichage des numeros des segments *)
  (* moveto ((xd + xo) / 2) ((yd + yo) / 2); *)
  (* draw_string x.id; *)
  draw_segments [|xo, yo, xd, yd|]

let draw3D x =
  let co, zlo, zuo = iof x.co, iof x.zlo, iof x.zuo in
  let cd, zld, zud = iof x.cd, iof x.zld, iof x.zud in
  (* DEBUG *)
  (* Format.eprintf "(%d, %d, %d) (%d, %d, %d)@." co zlo zuo cd zld zud; *)
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
  (* DEBUG *)
  (* Format.eprintf " %s (ci = %f, ce = %f)\n@." x.id x.ci x.ce; *)
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
(* DEBUG *)
(* Printf.printf "(x.co = %f, zlo = %f)\n(x.co = %f, zuo = %f)" *)
(* ^"\n(x.cd = %f, zud = %f)\n(x.cd = %f, zld = %f)\n" *)
(* x.co x.zlo x.co x.zuo x.cd x.zud x.cd x.zld; *)

let draw_miniplayer2D taille p =
  set_color black;
  let px = win_w / taille / 2 in
  fill_circle px 0 (10 / taille);
  let i = ref (140 / taille) in
  let etal = !i / 10 in
  let fov = fov / 2 in
  begin
    while !i > 10 do
      draw_arc px 0 !i !i (90 - fov) (90 + fov);
      i := !i - etal;
    done;
  end;
  set_color blue

let draw_player2D taille p =
  set_color black;
  let px = p.pos.x * win_w / 800 / taille in
  let py = p.pos.y * win_h / 800 / taille in
  fill_circle px py (10 / taille);
  let i = ref (140 / taille) in
  let etal = !i / 10 in
  let fov = fov / 2 in
  if minimap then
    begin
      while !i > 10 do
        draw_arc px py !i !i (p.pa - fov) (p.pa + fov);
        i := !i - etal;
      done;
    end
  else
    while !i > 10 do
      draw_arc px py !i !i (p.pa - fov) (p.pa + fov);
      i := !i - etal;
    done;
  set_color blue
            
let draw_player3D p =
  let yeux = get_hov() in
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

let rotate x y a p t =
  let c = dcos (-a) in
  let s = dsin (-a) in
  let px = foi ((win_w / 800) * 2) in
  let tx = foi x -. px in
  let ty = foi y -. 0. in
  (* DEBUG *)
  (* Format.eprintf "(px = %f, py = %f)\n@." px 0.; *)
  (iof (tx *. c -. ty *. s +. px), iof (tx *. s +. ty *. c +. 0.))  
    
let draw_mini2D x p =
  let taille = !scalemap in
  let a = p.pa in
  let xo, yo = (((iof x.xo) * win_w / 800) / taille), (((iof x.yo) * win_h / 800) / taille) in
  let xd, yd = (((iof x.xd) * win_w / 800) / taille), (((iof x.yd) * win_h / 800) / taille) in
  let px, py = p.pos.x * win_w / 800 / taille, p.pos.y * win_h / 800 / taille in
  let xo, yo = xo - px, yo - py in
  let xd, yd = xd - px, yd - py in
  let xo, yo = (rotate xo yo (a-90) p taille) in
  (* DEBUG *)
  (* Format.eprintf "(xo = %d, yo = %d)\n@." xo yo; *)
  let xd, yd = (rotate xd yd (a-90) p taille) in
  (* DEBUG *)
  (* Format.eprintf "(xd = %d, yd = %d)\n@." xd yd; *)
  draw_segments [|xo+(win_w/taille)/2, yo, xd+(win_w/taille)/2, yd|]
                
let draw_minimap map player =
  let taille = scale * 4 in
  set_line_width 2;
  (* contour *)
  set_color black;
  draw_rect 0 0 ((win_w + taille) / taille) ((win_h + taille) / taille);
  (* fond *)
  set_color (rgb 255 210 132);
  fill_rect 0 0 (win_w / taille) (win_h / taille);
  set_line_width 1;
  if get_map () = M1 then
    if player.pos.x >= 0
       && player.pos.y >=0
       && player.pos.x <= win_w
       && player.pos.y <= win_h
    then
      draw_player2D taille player
    else ()
  else
    draw_miniplayer2D taille player;
  set_color (rgb 102 51 0);
  set_line_width 2;
  scalemap := scale * 4;
  let f s = if get_map () = M2 then
              draw_mini2D (fsegment_of_seg s) player
            else
              draw2D (fsegment_of_seg s) player
  in
  parse f map player.pos;
  scalemap := scale

let draw_sun sun player landsky_inter =
  set_color yellow;
  if player.pa = 90 then
    begin
      sun.spos <- 0;
    end
  else if player.pa = 90 + 10 * 10 then
    begin
      sun.spos <- win_w
    end;
  if sun.ssize > 0 then
    fill_circle (sun.spos) (landsky_inter+win_h/4) (sun.ssize)

let draw_moon moon player landsky_inter =
  set_color (rgb 192 192 192);
  if player.pa = 90 then
    begin
      moon.mpos <- win_w;
    end
  else if player.pa = 90 + 10 * 10 then
    begin
      moon.mpos <- 0
    end;
  if moon.msize > 0 then
    fill_circle (moon.mpos) (landsky_inter+win_h/4) (moon.msize)
