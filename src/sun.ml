open Options

type t = {
    mutable spos : int;
    mutable ssize : int;
    mutable smove_s : int;
    mutable smove_r : int
  }
           
let new_sun spos ssize = {
    spos;
    ssize;
    smove_s = truncate step_dist;
    smove_r = win_w / 10
  }

let change_smove_r s =
  s.smove_r <- win_w / 10
