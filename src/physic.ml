open Bsp
open Segment
open Point
             
let lstocl s1 s2 =
  let bo = get_position s2.porig s1 in
  let bd = get_position s2.pdest s1 in
  match bo, bd with
  | C, _ -> true
  | _, C -> true
  | R, L -> true
  | L, R -> true
  | _ -> false
           
let intersect s1 s2 = lstocl s1 s2 && lstocl s2 s1
                                                                    
let detect_real s bsp =
  let rec rdc bsp =
    match bsp with
    | E -> false
    | N(r,ag, ad) ->
       let a = get_position s.porig r in
       let c = intersect s r in
       if c then true
       else
         match a with
         | C -> false
         | L -> rdc ag
         | R -> rdc ad
  in rdc bsp
         
let detect_collision p bsp =
  let rec rdc bsp =
    match bsp with
    | E -> false
    | N(r,ag, ad) ->
       let c = get_position p r in
       match c with
       | C -> true
       | L -> rdc ag
       | R -> rdc ad
  in rdc bsp
