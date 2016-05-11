type t = {
  id : string;
  porig : Point.t; 
  pdest : Point.t;
  lx : int;
  ly : int;
  ci : float;
  ce : float
}
           
type tpos = L | R | C

val new_segment : int -> int -> int -> int -> t

val get_segment : t -> t
  
val get_position : Point.t -> t -> tpos

val split_segment : t -> t -> t option * t option

val split : t -> t list -> t list * t list

val string_of_segment : t -> string

val get_id : unit -> string

val get_coordonnees : t -> float * float * float * float
