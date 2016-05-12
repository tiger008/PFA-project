open Options
open Graphics
open Player
open Parse_lab
open Segment
open Point
open Bsp
open Trigo
open Sys
open Render

let rec seglist il acc =
    match il with
  | [] -> acc
  | (xo, yo, xd, yd)::s -> seglist s ((new_segment xo yo xd yd)::acc)

let () =
  let ((x, y, a), sl) = read_lab (open_in argv.(1))  in
  let player = new_player (new_point x y) a win_w fov in
  let map = build_bsp (seglist sl []) in
    Printf.printf "%s\n" (string_of_bsp map);
  open_graph (Printf.sprintf " %dx%d" win_w win_h);
  auto_synchronize false;
  try
    display map player;
    draw_poly [|(0, 0); (0, 300); (300, 300); (300, 0)|];
    synchronize ();
    while true do
      let s = Graphics.wait_next_event [Graphics.Key_pressed] in
      let _ = match s.key with
        | 'w' | 'z' -> move MFwd player map
        | 's' -> move MBwd player map
        | 'a' | 'q'-> move MLeft player map
        | 'd' -> move MRight player map
        | 'e' -> rotate Left player
        | 'r' -> rotate Right player
        | '\027' -> raise Exit
        | _ -> ()
      in
      clear_graph ();
      display map player;
      synchronize ();
    done;
  with Exit -> close_graph (); exit (0)
