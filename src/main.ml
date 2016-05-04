open Options
open Graphics
open Player
open Parse_lab
open Segment
open Point
open Bsp

let rec seglist il acc =
    match il with
  | [] -> acc
  | (xo, yo, xd, yd)::s -> seglist s ((new_segment xo yo xd yd)::acc)

let () =
    let ((x, y, a), sl) = read_lab stdin in
    let player = new_player (new_point x y) a in
    let map = build_bsp (seglist sl []) in
    open_graph (Printf.sprintf " %dx%d" win_w win_h);
  auto_synchronize false;
  try
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
          (*display map player;*)
          clear_graph ();
          let anglebob = [(player.pos.x, player.pos.y, )] in
          draw_segments (Array.of_list sl);
          draw_circle player.pos.x player.pos.y 5;
          synchronize ();
      done;
          with Exit -> close_graph (); exit (0)
