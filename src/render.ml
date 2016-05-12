open Segment
open Point
open Player
open Trigo
open Graphics
open Bsp
open Fsegment
open Options

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

let clip2D l p =
  let rec rclip acc = function
    | [] -> acc
    | r::s ->
       let r = fsegment_of_seg r in
       let a = translation_rotation r p in
       let q = projection_h a p in
       if (a.xo < 1. && a.xd < 1.)
        || (q.yo < 0. && q.yd < 0.)
        || (q.yo > foi win_w && q.yd > foi win_w) then
         rclip acc s
       else if a.xo < 1. then
         let seg = {a with
                     xo = 1.;
                     yo = (a.yo +. (1. -. a.xo) *. (tan a.angle))} in
         let r = translation_rotation_inverse seg p in
         rclip (r::acc) s
       else if a.xd < 1. then
         let seg = {a with
                     xd = 1.;
                     yd = (a.yd +. (1. -. a.xd) *. (tan a.angle))} in
         let r = translation_rotation_inverse seg p in
         rclip (r::acc) s
       else rclip (r::acc) s
  in rclip [] l

let algo3D s =
  let ls = foi win_w in
  let du = (s.zud -. s.zuo) /. (s.cd -. s.co) in
  let dl = (s.zld -. s.zlo) /. (s.cd -. s.co) in
  let () = if s.co < 0. then
    begin
      s.co <- 0.;
      s.zuo <- s.zuo -. (s.co *. du);
      s.zlo <- s.zlo -. (s.co *. dl)
    end
  else if s.co > ls then
    begin
      s.co <- ls;
      s.zuo <- s.zuo -. ((s.co -. ls) *. du);
      s.zlo <- s.zlo -. ((s.co -. ls) *. dl)
    end
  else ()
    in
    if s.cd < 0. then
        begin
      s.cd <- 0.;
      s.zud <- s.zud -. (s.cd *. du);
      s.zld <- s.zld -. (s.cd *. dl)
    end
  else if s.cd > ls then
    begin
      s.cd <- ls;
      s.zud <- s.zud -. ((s.cd -. ls) *. du);
      s.zld <- s.zld -. ((s.cd -. ls) *. dl)
    end
  else ()

let projection_v s p =
  { s with
    zuo = (foi (win_h / 2)) +. (((foi ceiling_h) -. p.yeux) *. p.d /. s.xo);
    zlo = (foi (win_h / 2)) +. (((foi floor_h) -. p.yeux)  *. p.d /. s.xo);
    zud = (foi (win_h / 2)) +. (((foi ceiling_h) -. p.yeux) *. p.d /. s.xd);
    zld = (foi (win_h / 2)) +. (((foi floor_h) -. p.yeux)  *. p.d /. s.xd);
    co = (foi win_w) /. 2. -. s.yo *. p.d /. s.xo;
    cd = (foi win_w) /. 2. -. s.yd *. p.d /. s.xd
  }

let clip3D l p =
  let rec rclip acc = function
    | [] -> acc
    | r::s ->
       let r = fsegment_of_seg r in
       let tr = translation_rotation r p in
       let pv = projection_v tr p in
       let ph = projection_h pv p in
       if (tr.xo < 1. && tr.xd < 1.)
        || (ph.yo < 0. && ph.yd < 0.)
        || (ph.yo > foi win_w && ph.yd > foi win_w) then
         rclip acc s
       else if tr.xo < 1. then
         let seg = projection_h (projection_v ({tr with
           xo = 1.;
           yo = (tr.yo +. (1. -. tr.xo) *. (tan tr.angle))}) p) p
         in
         rclip (seg::acc) s
       else if tr.xd < 1. then
         let seg = projection_h (projection_v ({tr with
           xd = 1.;
           yd = (tr.yd +. (1. -. tr.xd) *. (tan tr.angle))}) p) p
         in
         rclip (seg::acc) s
       else rclip (ph::acc) s
  in rclip [] l

let rec bsp_to_list bsp p =
  let acc = ref [] in
  let f s = acc := s::!acc in
  rev_parse f bsp p;
  !acc

let rec draw2D taille = function
  | [] -> ()
  | x::s ->
    let xo, yo, xd, yd = (iof x.xo) / taille,
                         (iof x.yo) / taille,
                         (iof x.xd) / taille,
                         (iof x.yd) / taille
    in
    moveto ((xd + xo) / 2) ((yd + yo) / 2);
    draw_string x.id;
    draw_segments [|xo, yo, xd, yd|];
    draw2D taille s

let rec draw3D = function
  | [] -> ()
  | x::s ->
      algo3D x;
      set_color red;
      draw_poly ([|(iof x.co, iof x.zlo);
                   (iof x.co, iof x.zuo);
                   (iof x.cd, iof x.zud);
                   (iof x.cd, iof x.zld)|]);
      set_color blue;
      fill_poly ([|(iof x.co, iof x.zlo);
                   (iof x.co, iof x.zuo);
                   (iof x.cd, iof x.zud);
                   (iof x.cd, iof x.zld)|]);
      draw3D s

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

let draw_minimap map player taille =
  set_line_width 2;
  (* contour *)
  set_color blue;
  draw_rect 0 0 ((win_w + taille) / taille) ((win_h + taille) / taille);
  (* fond *)
  set_color magenta;
  fill_rect 0 0 (win_w / taille) (win_h / taille);
  set_color black;
  set_line_width 1;
  draw_player taille player;
  set_color blue;
  set_line_width 2;
  draw2D 4 map

let display bsp player =
  (* fond *)
  set_color green;
  fill_rect 0 0 win_w (win_h / 2);
  set_color white;
  fill_rect 0 (win_h / 2) win_w win_h;
  set_color black;
  let map3D = clip3D (bsp_to_list bsp player.pos) player in
  let map2D = List.rev_map fsegment_of_seg (bsp_to_list bsp player.pos) in
  draw3D map3D;
  draw_minimap map2D player 4;
  (* 2D only *)
  (* draw_player 1 player *)
