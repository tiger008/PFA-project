type t = {
    mutable spos : int;
    mutable ssize : int;
    mutable smove_s : int;
    mutable smove_r : int
  }

val new_sun : int -> int -> t

val change_smove_r : t -> unit
