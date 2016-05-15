open Options

type t = {
    mutable mpos : int;
    mutable msize : int;
    mutable mmove_s : int;
    mutable mmove_r : int
  }

let new_moon mpos msize = {
    mpos;
    msize;
    mmove_s = truncate step_dist;
    mmove_r = win_w / 10
  }

let change_mmove_r m =
  m.mmove_r <- win_w / 10
