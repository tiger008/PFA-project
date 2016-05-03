open Point

type t = {id : string; 
          porig : Point.t;
          pdest : Point.t;
          ci : float;
          ce : float;
         }
           
type tpos = L | R | C
                      
let compteur i =
    let etat = ref i in
    fun () -> etat := !etat + 1; string_of_int !etat

let get_id = compteur 0

let new_segment xo yo xd yd = { id = get_id (); porig = Point.new_point xo yo; pdest = Point.new_point xd yd; ci = 0.; ce = 1. }

let get_position p s =
    let z = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x) in
    match z with
     | 0 -> C
     | _ when z < 0 -> R
     | _ -> L

let get_position2 p s =
    let z = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x) in
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
  let d = (s1.pdest.x - s1.porig.x) * (s2.pdest.y - s2.porig.y) - (s1.pdest.y - s1.porig.y) * (s2.pdest.x - s2.porig.x) in
  let (poso, z) = get_position2 s2.porig s1 in
  let (posd, _) = get_position2 s2.pdest s1 in
  match d, poso with
  | 0, L -> Some s2, None
  | 0, R -> None, Some s2
  | _, C when posd = L -> Some s2, None
  | _, C when posd = R -> None, Some s2
  | _, C -> None, None
  | _ -> let c = (float_of_int (-z))/.(float_of_int d) in
         match c, poso with
         | 0., _ -> posof posd s2
         | _, _ when c < 0. && c >= 1. -> posof poso s2
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
