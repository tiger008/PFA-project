type t = {
    id : string;
    xo : float;
    yo : float;
    xd : float;
    yd : float;
    ci : float;
    ce : float;
    angle : float;
    mutable zuo : float;
    mutable zlo : float;
    mutable zud : float;
    mutable zld : float;
    mutable co : float;
    mutable cd : float
}

val new_fsegment : float -> float -> float -> float -> t
val fsegment_of_seg : Segment.t -> t
