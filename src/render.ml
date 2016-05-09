open Segment
open Point
open Player
open Trigo
open Graphics
open Bsp
       
let tr s p =
  {s with
    porig = new_point (iof ((foi (s.porig.x - p.pos.x)) *. (dcos (-p.pa)) -. (foi (s.porig.y - p.pos.y)) *. (dsin (-p.pa)))) (iof ((foi (s.porig.y - p.pos.y)) *. (dcos (-p.pa)) +. (foi (s.porig.x - p.pos.x)) *. (dsin (-p.pa))));
    pdest = new_point (iof ((foi (s.pdest.x - p.pos.x)) *. (dcos (-p.pa)) -. (foi (s.pdest.y - p.pos.y)) *. (dsin (-p.pa)))) (iof ((foi (s.pdest.y - p.pos.y)) *. (dcos (-p.pa)) +. (foi (s.pdest.x - p.pos.x)) *. (dsin (-p.pa))))
  }
    
let itr s p =
  {s with
    porig = translation (rotation s.porig p.pa) (new_point p.pos.x p.pos.y);
    pdest = translation (rotation s.pdest p.pa) (new_point p.pos.x p.pos.y)
  }

let angle s =
    atan (foi (s.pdest.y - s.porig.y)) /. (foi (s.pdest.x - s.porig.x))

let clip bsp p =
  let rec rclip acc = function
    | E -> acc
    | N(r, ag, ad) -> let r = get_segment r in let a = tr r p in
      if a.porig.x < 1 && a.pdest.x < 1 then
        List.rev_append (rclip acc ag) (rclip acc ad)
      else if a.porig.x < 1 then
        let seg = {a with porig = new_point 1 (iof (foi (a.porig.y + (1 - a.porig.x)) *. tan ((angle a)))); pdest = new_point a.pdest.x a.pdest.y} in
        List.rev_append (rclip ((itr seg p)::acc) ag) (rclip acc ad)
      else if a.porig.y < 1 then
        let seg = {a with porig = new_point a.porig.x a.porig.y; pdest = new_point 1 (iof (foi (a.pdest.y + (1 - a.pdest.x)) *. tan ((angle a))))} in
        List.rev_append (rclip ((itr seg p)::acc) ag) (rclip acc ad)
      else List.rev_append (rclip (r::acc) ag) (rclip acc ad)
  in rclip [] bsp
  
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
  let map = clip bsp player in
  (*let rec recd = function
    | E -> ()
    | N(r,ag,ad) -> r; recd ag; recd ad
    in recd map*)
  draw map

                                
