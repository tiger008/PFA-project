type t = {
    id : string;
    xo : float;
    yo : float;
    xd : float;
    yd : float;
    angle : float
}

val new_fsegment : float -> float -> float -> float -> t
val fsegment_of_seg : Segment.t -> t
