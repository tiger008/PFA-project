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
