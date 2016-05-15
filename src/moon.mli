type t = {
    mutable mpos : int;
    mutable msize : int;
    mutable mmove_s : int;
    mutable mmove_r : int
  }

val new_moon : int -> int -> t

val change_mmove_r : t -> unit
