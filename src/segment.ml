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
    let z = (s.porig.x + iof ((foi s.lx) *. s.ce) - s.porig.x + iof((foi s.lx) *. s.ci)) * (p.y - s.porig.y  + iof((foi s.ly) *. s.ci)) - (s.porig.y + iof((foi s.ly) *. s.ce) - s.porig.y + iof((foi s.ly) *. s.ci)) * (p.x - s.porig.x + iof((foi s.lx) *. s.ci)) in
    match z with
     | 0 -> C
     | _ when z < 0 -> R
     | _ -> L

let get_position2 p s =
    let z = (s.porig.x + iof ((foi s.lx) *. s.ce) - s.porig.x + iof((foi s.lx) *. s.ci)) * (p.y - s.porig.y  + iof((foi s.ly) *. s.ci)) - (s.porig.y + iof((foi s.ly) *. s.ce) - s.porig.y + iof((foi s.ly) *. s.ci)) * (p.x - s.porig.x + iof((foi s.lx) *. s.ci)) in
    match z with
     | 0 -> C, z
     | _ when z < 0 -> R, z
     | _ -> L, z

let posof tp s =
  match tp with
  | L -> Some s, None
  | R -> None, Some s
  | _ -> None, None

let split_segment s1 s2 =
  let d = (s1.porig.x + iof ((foi s1.lx) *. s1.ce) - s1.porig.x + iof((foi s1.lx) *. s1.ci)) * (s2.porig.y + iof((foi s2.ly) *. s2.ce) - s2.porig.y + iof((foi s2.ly) *. s2.ci)) - (s1.porig.y + iof((foi s1.ly) *. s1.ce) - s1.porig.y + iof((foi s1.ly) *. s1.ci)) * (s2.porig.x + iof ((foi s2.lx) *. s2.ce) - s2.porig.x + iof((foi s2.lx) *. s2.ci)) in
  let (poso, z) = get_position2 s2.porig s1 in
  let posd = get_position s2.pdest s1 in
  match d, poso with
  | 0, L -> Some s2, None
  | 0, R -> None, Some s2
  | _, C when posd = L -> Some s2, None
  | _, C when posd = R -> None, Some s2
  | _, C -> None, None
  | _ -> let c = (foi (-z)) /. (foi d) in
         match c, poso with
         | 0., _ -> posof posd s2
         | _, _ when c < 0. || c >= 1. -> posof poso s2
         | _ -> Some { s2 with ce = c }, Some { s2 with ci = c }

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
