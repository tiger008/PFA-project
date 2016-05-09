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

let vrai_point1 s =
  new_point (iof ((foi s.porig.x) +. s.ci *. (foi s.lx))) (iof ((foi s.porig.y) +. s.ci *. (foi s.ly)))

let vrai_point2 s =
  new_point (iof ((foi s.pdest.x) +. s.ce *. (foi s.lx))) (iof ((foi s.pdest.y) +. s.ce *. (foi s.ly)))
       
let translation_rotation s p =
  {s with
    porig = rotation (translation (vrai_point1 s) (new_point (-p.pos.x) (-p.pos.y))) (-p.pa);
    pdest = rotation (translation (vrai_point2 s) (new_point (-p.pos.x) (-p.pos.y))) (-p.pa);
  }
            
let translation_rotation_inverse s p =
  {s with
    porig = translation (rotation (vrai_point1 s) p.pa) (new_point (-p.pos.x) (-p.pos.y));
    pdest = translation (rotation (vrai_point2 s) p.pa) (new_point (-p.pos.x) (-p.pos.y))
  }

let angle s =
    atan2 (((foi s.pdest.y) +. s.ce *. (foi s.ly)) -. ((foi s.porig.y) +. s.ci *. (foi s.ly))) (((foi s.pdest.x) +. s.ce *. (foi s.lx)) -. ((foi s.porig.x) +. s.ci *. (foi s.lx)))

let clip l p =
  let rec rclip acc = function
    | [] -> acc
    | r::s -> let a = translation_rotation r p in
              if a.porig.x < 1 && a.pdest.x < 1 then
                rclip acc s
              else if a.porig.x < 1 then
                let seg = {a with porig = new_point 1 (iof (foi (a.porig.y + (1 - a.porig.x)) *. tan ((angle a)))); pdest = new_point a.pdest.x a.pdest.y} in
                rclip ((translation_rotation_inverse seg p)::acc) s
              else if a.porig.y < 1 then
                let seg = {a with porig = new_point a.porig.x a.porig.y; pdest = new_point 1 (iof (foi (a.pdest.y + (1 - a.pdest.x)) *. tan ((angle a))))} in
                rclip ((translation_rotation_inverse seg p)::acc) s
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
    Array.of_list [(s.porig.x + iof (s.ci *. (foi s.lx)), s.porig.y + iof (s.ci *. (foi s.ly)), s.porig.x + iof (s.ce *. (foi s.lx)), s.porig.y + iof (s.ce *. (foi s.ly)))]

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

                                
