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

let new_segment xo yo xd yd = { id = get_id (); porig = Point.new_point xo yo; pdest = Point.new_point xd yd; ci = 0.; ce = 100. }

let get_position p s =
  let open Point in
  let z = (s.pdest.x - s.porig.x) * (p.y - s.porig.y) - (s.pdest.y - s.porig.y) * (p.x - s.porig.x) in
  match z with
  | 0 -> C
  | _ when z < 0 -> R
  | _ -> L
           
let split_segment s1 s2 = 

let split hd rest = failwith "TODO"
