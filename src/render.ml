open Bsp
open Draw
open Fsegment
open Graphics
open Options
open Player
open Point
open Segment
open Trigo

let win_w = foi win_w
let win_h = foi win_h
let ceiling_h = foi ceiling_h
let floor_h = foi floor_h
let scalemap = ref scale
            
let clip2D r p =
  let r = fsegment_of_seg r in
  match translation_rotation r p with
  | None -> ()
  | Some tr ->
     match projection_h tr p with
     | None -> ()
     | Some ph -> draw2D (translation_rotation_inverse tr p) p

let algo3D s =
  let ls = win_w in
  let ymax = 20000. in
  let ymin = -.ymax in
  let du = (s.zud -. s.zuo) /. (s.cd -. s.co) in
  let dl = (s.zld -. s.zlo) /. (s.cd -. s.co) in
  (* DEBUG *)
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
  else if s.zlo = ymin && s.zuo = ymax && s.co > s.cd then s.co <- win_w;
  if s.zld = ymin && s.zud = ymax && s.cd < s.co then s.cd <- 0.
  else if s.zld = ymin && s.zud = ymax && s.cd > s.co then s.cd <- win_w
(* ; *)
(* DEBUG *)
(* Format.eprintf "(%.1f, %.1f, %.1f) (%.1f, %.1f, %.1f)@,\ *)
(*                 ---------------------@." *)
(*                s.co s.zlo s.zuo s.cd s.zld s.zud *)
                                                                     
let clip3D p r =
  let r = fsegment_of_seg r in
  (* DEBUG *)
  (* let xo, yo = iof r.xo, iof r.yo in *)
  (* let xd, yd = iof r.xd, iof r.yd in *)
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
      set_color (rgb 255 210 132);
      fill_rect 0 0 win_w win_h;
      set_color black;
      draw_player2D scale player;
      set_color (rgb 102 51 0);
      let f s = clip2D s player in
      rev_parse f bsp player.pos;
    end
  else
    begin
      let landsky_inter = win_h / 2 + get_hov () in
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
      (* DEBUG *)
      (* Format.eprintf "-----------------@."; *)
      if minimap then draw_minimap bsp player;
      if get_hov () = ceiling_h / 2 && get_perspective () = RPG then
        draw_player3D player
      else ()
    end
