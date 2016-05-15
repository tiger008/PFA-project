val rtan : float -> float

val dtan : int -> float

val dcos : int -> float

val dacos : float -> float

val dsin : int -> float

val rotation : Fsegment.t -> int -> Fsegment.t

val translation : Fsegment.t -> Point.t -> Fsegment.t

val r_to_deg : float -> float

val calc_angle : Fsegment.t -> float

val translation_rotation : Fsegment.t -> Player.t -> Fsegment.t option

val translation_rotation_inverse : Fsegment.t -> Player.t -> Fsegment.t

val projection_h : Fsegment.t -> Player.t -> Fsegment.t option

val projection_v : Fsegment.t -> Player.t -> Fsegment.t

val between : 'a -> 'a -> 'a -> bool

val lstocl : Segment.t -> Segment.t -> bool

val intersect : Segment.t -> Segment.t -> bool
