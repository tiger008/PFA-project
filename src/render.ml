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

let projection s p =
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
       Format.eprintf "zc =  %f, zf = %f@." a.zud a.zuo; 
       let q = projection a p in
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

let segment_vertices s p =
  { s with
    zuo = (foi (win_h / 2)) +. ((((foi ceiling_h) -. step_dist) *. p.d) /. s.xo);
    zlo = (foi (win_h / 2)) +. ((((foi floor_h) -. step_dist)  *. p.d) /. s.xo);
    zud = (foi (win_h / 2)) +. ((((foi ceiling_h) -. step_dist) *. p.d) /. s.xd);
    zld = (foi (win_h / 2)) +. ((((foi floor_h) -. step_dist)  *. p.d) /. s.xd);
    co = (foi win_w) /. 2. -. s.yo *. p.d /. s.xo;
    cd = (foi win_w) /. 2. -. s.yd *. p.d /. s.xd
  }

let clip3D l p =
  let rec rclip acc = function
    | [] -> acc
    | r::s ->
       let r = fsegment_of_seg r in
       let a1 = translation_rotation r p in
       let a = segment_vertices a1 p in
       let q = projection a p in
       if (a.xo < 1. && a.xd < 1.)
        || (q.yo < 0. && q.yd < 0.)
        || (q.yo > foi win_w && q.yd > foi win_w) then
         rclip acc s
       else if a.xo < 1. then
         let seg = {a with
           xo = 1.;
           yo = (a.yo +. (1. -. a.xo) *. (tan a.angle))} in
         rclip (seg::acc) s
       else if a.xd < 1. then
         let seg = {a with
           xd = 1.;
           yd = (a.yd +. (1. -. a.xd) *. (tan a.angle))} in
         rclip (seg::acc) s
       else rclip (a::acc) s
  in rclip [] l  

let rec bsp_to_list = function
  | E -> []
  | N(r,ag,ad) ->
    List.rev_append [r] (List.rev_append (bsp_to_list ag) (bsp_to_list ad))

let rec draw2D taille = function
  | [] -> ()
  | x::s ->
    let xo, yo, xd, yd = (iof x.xo), (iof x.yo), (iof x.xd), (iof x.yd) in
    moveto ((xd / taille + xo / taille) / 2) ((yd / taille + yo / taille) / 2);
    draw_string x.id;
    draw_segments [|xo / taille, yo / taille, xd / taille, yd / taille|];
    draw2D taille s

let rec draw3D = function
    | [] -> ()
    | x::s -> fill_poly ([|(iof x.xo, iof x.zlo);
                           (iof x.xo, iof x.zuo);
                           (iof x.xd, iof x.zld);
                           (iof x.xd, iof x.zud)|]);
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
  set_line_width 3;
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
  set_line_width 3;
  draw2D 4 map

let display bsp player =
  (* fond *)
  set_color green;
  fill_rect 0 0 win_w (win_h / 2);
  set_color white;
  fill_rect 0 (win_h / 2) win_w win_h;
  draw_player 1 player;
  let map = clip2D (bsp_to_list bsp) player in
  draw_minimap map player 4;
  draw2D 1 map
