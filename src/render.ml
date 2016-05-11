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
    yo = ((foi win_w) /. 2.) -. (s.yo *. p.d /. s.xo);
    xd = p.d;
    yd = ((foi win_w) /. 2.) -. (s.yd *. p.d /. s.xd)
    }


let clip l p =
  let rec rclip acc = function
    | [] -> acc
    | r::s ->
       let r = fsegment_of_seg r in
       let a = translation_rotation r p in
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

let rec bsp_to_list = function
  | E -> []
  | N(r,ag,ad) ->
     List.rev_append [r] (List.rev_append (bsp_to_list ag) (bsp_to_list ad))

let rec draw = function
  | [] -> ()
  | x::s ->
    let xo, yo, xd, yd = (iof x.xo), (iof x.yo), (iof x.xd), (iof x.yd) in
    (*Format.eprintf "%d, %d, %d, %d@." xo yo xd yd;*)
    moveto ((xd + xo) / 2) ((yd + yo) / 2);
    draw_string x.id;
    draw_segments [|xo, yo, xd, yd|];
    draw s

let draw_player p =
    fill_circle p.pos.x p.pos.y 10;
    set_color blue;
    let i = ref 140 in
    let etal = !i / 10 in
    while !i > 10 do
        draw_arc p.pos.x p.pos.y !i !i (p.pa - fov / 2) (p.pa + fov / 2);
        i := !i - etal;
    done;
    set_color black


let display bsp player =
  let map = clip (bsp_to_list bsp) player in
  draw_player player;
  draw map
