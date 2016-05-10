open Segment
open Point
open Player
open Trigo
open Graphics
open Bsp

(*let translation_rotation s p =
  {s with
  porig = new_point (iof ((foi (s.porig.x - p.pos.x)) *. (dcos (-p.pa)) -. (foi (s.porig.y - p.pos.y)) *. (dsin (-p.pa)))) (iof ((foi (s.porig.y - p.pos.y)) *. (dcos (-p.pa)) +. (foi (s.porig.x - p.pos.x)) *. (dsin (-p.pa))));
  pdest = new_point (iof ((foi (s.pdest.x - p.pos.x)) *. (dcos (-p.pa)) -. (foi (s.pdest.y - p.pos.y)) *. (dsin (-p.pa)))) (iof ((foi (s.pdest.y - p.pos.y)) *. (dcos (-p.pa)) +. (foi (s.pdest.x - p.pos.x)) *. (dsin (-p.pa))))
  }*)

let translation_rotation s p =
  {s with
    porig = rotation (translation s.porig (new_point (-p.pos.x) (-p.pos.y))) (-p.pa);
    pdest = rotation (translation s.pdest (new_point (-p.pos.x) (-p.pos.y))) (-p.pa);
  }

let translation_rotation_inverse s p =
  {s with
    porig = translation (rotation s.porig p.pa) p.pos;
    pdest = translation (rotation s.pdest p.pa) p.pos
  }

let angle s =
  atan2 (foi (s.pdest.y - s.porig.y)) (foi (s.pdest.x - s.porig.x))

let clip l p =
  let rec rclip acc = function
    | [] -> acc
    | r::s ->
      let r = get_segment r in
      let xo, yo, xd, yd = r.porig.x, r.porig.y, r.pdest.x, r.pdest.y in
      Format.eprintf "r : %d, %d, %d, %d@." xo yo xd yd;
      let a = translation_rotation r p in
      let xo, yo, xd, yd = a.porig.x, a.porig.y, a.pdest.x, a.pdest.y in
      Format.eprintf "a : %d, %d, %d, %d@." xo yo xd yd;
      let r = translation_rotation_inverse a p in
      if a.porig.x < 1 && a.pdest.x < 1 then
        rclip acc s
          
      else let aa = angle a in
           let ta = tan aa in
           Format.eprintf "angle : %.1f, %.1f@." (r_to_deg aa) ta;
           if a.porig.x < 1 then
             let seg = {a with
               porig = new_point 1 (iof (foi a.porig.y +. (1. -. foi a.porig.x) *. ta))} in
             let xo, yo, xd, yd = seg.porig.x, seg.porig.y, seg.pdest.x, seg.pdest.y in
             Format.eprintf "seg : %d, %d, %d, %d@." xo yo xd yd;
             let r = translation_rotation_inverse seg p in
             let xo, yo, xd, yd = r.porig.x, r.porig.y, r.pdest.x, r.pdest.y in
             Format.eprintf "Clip or < 1 : %d, %d, %d, %d@." xo yo xd yd;
             rclip (r::acc) s
           else if a.pdest.x < 1 then
             let seg = {a with porig = a.porig; pdest = new_point 1 (iof (foi a.pdest.y +. (1. -. foi a.pdest.x) *. ta))} in
             let r = translation_rotation_inverse seg p in
             let xo, yo, xd, yd = r.porig.x, r.porig.y, r.pdest.x, r.pdest.y in
             Format.eprintf "Clip de < 1 : %d, %d, %d, %d@." xo yo xd yd;
             rclip (r::acc) s
           else rclip (r::acc) s
  in rclip [] l
  
let rec bsp_to_list = function
  | E -> []
  | N(r,ag,ad) ->
    List.rev_append [r] (List.rev_append (bsp_to_list ag) (bsp_to_list ad))

(*let rec array_segments bsp acc =
  match bsp with
  | E -> acc
  | N(s,sl,sr) -> 
  let draw_player*)

let segtoarray s =
  let xo, yo, xd, yd = s.porig.x, s.porig.y, s.pdest.x, s.pdest.y in
  Format.eprintf "%d, %d, %d, %d@." xo yo xd yd;
  [|xo, yo, xd, yd|]

let rec draw sl =
  match sl with
  | [] -> ()
  | x::s -> draw_segments (segtoarray x); draw s
    
let display bsp player =
  let angle1 = [(player.pos.x, player.pos.y, player.pos.x + (iof (10. *. (dcos (player.pa-30)))), player.pos.y + (iof (20. *. (dsin (player.pa-30)))))] in
  let angle2 = [(player.pos.x, player.pos.y, player.pos.x + (iof (10. *. (dcos (player.pa+30)))), player.pos.y + (iof (20. *. (dsin (player.pa+30)))))] in
  draw_segments (Array.of_list angle1);
  draw_segments (Array.of_list angle2);
  let map = clip (bsp_to_list bsp) player in
  draw map
