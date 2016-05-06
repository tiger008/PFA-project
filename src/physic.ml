open Bsp
open Segment
open Point

let box_intersect s1 s2 = s1.porig.x <= s2.pdest.x && s1.pdest.x >= s2.porig.x && s1.porig.y <= s2.pdest.y && s1.pdest.y >= s2.porig.y

let product p1 p2 = p1.x * p2.y - p2.x * p1.y

let iob = function
    | true -> 1
    | _ -> 0

let boi = function
    | 0 -> false
    | _ -> true

let lstocl s1 s2 =
    let bo = get_position s2.porig s1 in
    let bd = get_position s2.pdest s1 in
    match bo, bd with
    | C, _ -> true
    | _, C -> true
    | R, e when e != R -> true
    | e, R when e != R -> true
    | _ -> false

let intersect2 s1 s2 = box_intersect s1 s2 && lstocl s1 s2 && lstocl s2 s1


let intersect s1 s2 =
  if (s1.porig.x = s2.porig.x && s1.porig.y = s2.porig.y)
     || (s1.pdest.x = s2.pdest.x && s1.pdest.y = s2.pdest.y)
  then true
  else
    if s1.porig.x - s1.pdest.x = 0 then false else
      let a1 = (s1.porig.y-s1.pdest.y)/(s1.porig.x-s1.pdest.x) in
      let b1 = s1.porig.y - a1 * s1.porig.x in
      if s2.porig.x - s2.pdest.x = 0 then false else
        let a2 = (s2.porig.y-s2.pdest.y)/(s2.porig.x-s2.pdest.x) in
        let b2 = s2.porig.y - a2 * s2.porig.x in
        if a1-a2 = 0 then false
        else
          let co = (b2 - b1) / (a1 - a2) in
          if s1.porig.x <= co && co <= s1.pdest.x && s2.porig.x <= co && co <= s2.pdest.x then true
          else false

let detect_real s bsp =
  let rec rdc bsp =
    match bsp with
    | E -> false
    | N(r,ag, ad) ->
       let a = get_position s.porig r in
       let c = intersect2 s r in
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

     
