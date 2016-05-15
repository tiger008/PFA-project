open Point
       
type t = {id : string;
          porig : Point.t;
          pdest : Point.t;
          lx : int;
          ly : int;
          ci : float;
          ce : float
         }
           
type tpos = L | R | C
                      
let compteur i =
  let etat = ref i in
  fun () -> etat := !etat + 1; string_of_int !etat
                                             
let get_id = compteur 0

let new_segment xo yo xd yd = {
    id = get_id ();
    porig = Point.new_point xo yo;
    pdest = Point.new_point xd yd;
    lx = xd - xo;
    ly = yd - yo;
    ci = 0.;
    ce = 1.
  }

let get_segment s =
  {s with
    porig = new_point (s.porig.x + iof ((foi s.lx) *. s.ci))
                      (s.porig.y + iof ((foi s.ly) *. s.ci));
    pdest = new_point (s.porig.x + iof ((foi s.lx) *. s.ce))
                      (s.porig.y + iof ((foi s.ly) *. s.ce))
  }

let get_coordonnees s =
  ((foi s.porig.x) +. (foi s.lx) *. s.ci,
   (foi s.porig.y) +. (foi s.ly) *. s.ci,
   (foi s.porig.x) +. (foi s.lx) *. s.ce,
   (foi s.porig.y) +. (foi s.ly) *. s.ce)

let string_of_segment s =
  Printf.sprintf "[%s (%d, %d) (%d, %d) (%.1f, %.1f)]"
                 s.id s.porig.x s.porig.y s.pdest.x s.pdest.y s.ci s.ce

let print_segment fmt s =
  Format.fprintf fmt "%s" (string_of_segment s)
                 
let get_position p s =
  let xo, yo, xd, yd = s.porig.x, s.porig.y, s.pdest.x, s.pdest.y in
  let z = (xd - xo) * (p.y - yo)
          - (yd - yo) * (p.x - xo) in
  if z = 0 then C
  else if z < 0 then R
  else L
         
let get_position2 (px, py) s =
  let xo, yo, xd, yd = get_coordonnees s in
  let z = (xd -. xo) *. (py -. yo)
          -. (yd -. yo) *. (px -. xo) in
  if z = 0. then C, z
  else if z < 0. then R, z
  else L, z

let posof tp s =
  match tp with
  | L -> Some s, None
  | _ -> None, Some s (* A droite par principe s'il est confondu *)

let split_segment s1 s2 =
  let xo, yo, xd, yd = get_coordonnees s1 in
  let xo2, yo2, xd2, yd2 = get_coordonnees s2 in
  let d = (xd -. xo) *. (yd2 -. yo2)
          -. (yd -. yo) *. (xd2 -. xo2) in
  let (poso, z) = get_position2 (xo2, yo2) s1 in
  if d = 0. then
    match poso with
    | R | C -> None, Some s2 (* à droite ou colinéaire *)
    | L -> Some s2, None
  else let c = (-.z) /. d in
       if c = 0. then
         let (posd, _) = get_position2 (xd2, yd2) s1 in
         posof posd s2
       else if c < 0. || c >= 1. then posof poso s2
       else
         match poso with
         | R | C -> Some { s2 with id = Printf.sprintf "%sl" s2.id; ci = c },
                    Some { s2 with id = Printf.sprintf "%sr" s2.id; ce = c }
         | L -> Some { s2 with id = Printf.sprintf "%sl" s2.id; ce = c },
                Some { s2 with id = Printf.sprintf "%sr" s2.id; ci = c }

let split s sl =
  let rec srec (sll, slr) sl =
    match sl with
    | [] -> sll, slr
    | x::f -> let l, r = split_segment s x in
              match l, r with
              | None, None -> srec (sll, slr) f
              | None, Some d -> srec (sll, d::slr) f
              | Some g, None -> srec (g::sll, slr) f
              | Some g, Some d -> srec (g::sll, d::slr) f
  in
  let l, r = srec ([], []) sl in
  (* DEBUG *)
  (* Format.eprintf "Splitter : %a@." print_segment s; *)
  (* Format.eprintf "Gauche : "; *)
  (* List.iter (Format.eprintf "%a@." print_segment) l;  *)
  (* Format.eprintf "Droite : "; *)
  (* List.iter (Format.eprintf "%a@." print_segment) r; *)
  (* Format.eprintf "--------------@."; *)
  l, r

