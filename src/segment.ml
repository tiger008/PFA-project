open Point

type t = {id : string;
          porig : Point.t;
          pdest : Point.t;
          lx : int;
          ly : int;
          ci : float;
          ce : float;
         }
type tpos = L | R | C

let compteur i =
    let etat = ref i in
    fun () -> etat := !etat + 1; string_of_int !etat

let get_id = compteur 0

let new_segment xo yo xd yd = { id = get_id (); porig = Point.new_point xo yo; pdest = Point.new_point xd yd; lx = xd - xo; ly = yd - yo; ci = 0.; ce = 1. }

let get_segment s =
  {s with porig = new_point (s.porig.x + iof ((foi s.lx) *. s.ci)) (s.porig.y + iof ((foi s.ly) *. s.ci)); pdest = new_point (s.porig.x + iof ((foi s.lx) *. s.ce)) (s.porig.y + iof ((foi s.ly) *. s.ce))}

let get_position p s =
  let s = get_segment s in
    let z = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x) in
    match z with
     | 0 -> C
     | _ when z < 0 -> R
     | _ -> L

let get_position2 p s =
  let s = get_segment s in
    let z = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x) in
  (*  let z = (s.porig.x + iof ((foi s.lx) *. s.ce) - s.porig.x + iof((foi s.lx) *. s.ci)) * (p.y - s.porig.y  + iof((foi s.ly) *. s.ci)) - (s.porig.y + iof((foi s.ly) *. s.ce) - s.porig.y + iof((foi s.ly) *. s.ci)) * (p.x - s.porig.x + iof((foi s.lx) *. s.ci)) in *)
    match z with
     | 0 -> C, z
     | _ when z < 0 -> R, z
     | _ -> L, z

let posof tp s =
  match tp with
  | L -> Some s, None
  | _ -> None, Some s (* A droite par principe s'il est confondu *)

let split_segment s1 s2 =
  let ss1 = get_segment s1 in
  let ss2 = get_segment s2 in
  let d = (ss1.pdest.x - ss1.porig.x) * (ss2.pdest.y - ss2.porig.y) - (ss1.pdest.y - ss1.porig.y) * (ss2.pdest.x - ss2.porig.x) in
  let (poso, z) = get_position2 s2.porig s1 in
  let posd = get_position s2.pdest s1 in
  match d, poso with
  | 0, R -> None, Some s2
  | 0, L -> Some s2, None
  | 0, C -> None, Some s2 (* Colinéaire *)
  | _ -> let c = (foi (-z)) /. (foi d) in
         match c with
         | 0. -> posof posd s2
         | _ when c < 0. || c >= 1. -> posof poso s2
         | _ -> Some { s2 with ce = c }, Some { s2 with ci = c }

  (* let d = (s1.porig.x + iof ((foi s1.lx) *. s1.ce) - s1.porig.x + iof((foi s1.lx) *. s1.ci)) * (s2.porig.y + iof((foi s2.ly) *. s2.ce) - s2.porig.y + iof((foi s2.ly) *. s2.ci)) - (s1.porig.y + iof((foi s1.ly) *. s1.ce) - s1.porig.y + iof((foi s1.ly) *. s1.ci)) * (s2.porig.x + iof ((foi s2.lx) *. s2.ce) - s2.porig.x + iof((foi s2.lx) *. s2.ci)) in
  let (poso, z) = get_position2 s2.porig s1 in
  let posd = get_position s2.pdest s1 in
  match d, poso with
  | 0, L -> Some s2, None
  | 0, R -> None, Some s2
  | _, C when posd = L -> Some s2, None
  | _, C when posd = R -> None, Some s2
  | _, C -> None, Some s2
  | _ -> let c = (foi (-z)) /. (foi d) in
         match c, poso with
         | 0., _ -> posof posd s2
         | _, _ when c < 0. || c >= 1. -> posof poso s2
         | _ -> Some { s2 with ce = c }, Some { s2 with ci = c }
*)
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
  in srec ([], []) sl

let string_of_segment s =
  "[id : "^s.id^", (xo : "^(string_of_int s.porig.x)^", yo : "^(string_of_int s.porig.y)^"), (xd : "^(string_of_int s.pdest.x)^", yd : "^(string_of_int s.pdest.y)^"), (lx : "^(string_of_int s.lx)^", ly : "^(string_of_int s.ly)^"), (ci : "^(string_of_float s.ci)^", ce : "^(string_of_float s.ce)^")]"
