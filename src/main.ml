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
open Sun
open Moon

let rec seglist il acc =
    match il with
  | [] -> acc
  | (xo, yo, xd, yd)::s -> seglist s ((new_segment xo yo xd yd)::acc)

let touche = function
  | FR -> ('z', 's', 'q', 'd', 'a', 'e')
  | _ -> ('w', 's', 'a', 'd', 'q', 'e')

let () =
  let ((x, y, a), sl) = read_lab (open_in argv.(1))  in
  let player = new_player (new_point x y) a win_w fov in
  let map = build_bsp (seglist sl []) in
  let sun = new_sun 0 (50 + player.pos.y/win_h) in
  let moon = new_moon win_w (40 + player.pos.y/win_h) in
  (*
    (* DEBUG *)
    Printf.printf "%s\n" (string_of_bsp map);
  *)
  open_graph (Printf.sprintf " %dx%d" win_w win_h);
  auto_synchronize false;
  try
    display map player sun moon;
    synchronize ();
    while true do
      let (fw, bw, left, right, rleft, rright) = touche (get_lang ()) in
      let s = Graphics.wait_next_event [Graphics.Key_pressed] in
      let _ = match s.key with
        | _ when s.key = fw -> move MFwd player map
        | _ when s.key = bw -> move MBwd player map
        | _ when s.key = left-> move MLeft player map
        | _ when s.key = right -> move MRight player map
        | _ when s.key = rleft -> rotate Left player;
                                  sun.spos <- sun.spos + 5 + fov;
                                  moon.mpos <- moon.mpos -5 - fov
        | _ when s.key = rright -> rotate Right player;
                                   Format.eprintf "%d\n@." player.pa;
                                   sun.spos <- sun.spos - 5 - fov;
                                   moon.mpos <- moon.mpos + 5 + fov
        | 'l' -> change_lang (get_lang ())
        | 'c' -> change_mode (get_mode ())
        | 'p' -> increment_hov (); change_yeux player (get_hov ())
        | 'o' -> decrement_hov (); change_yeux player (get_hov ())
        | 't' -> change_time (get_time ())
        | 'v' -> change_perspective (get_perspective ())
        | '\027' -> raise Exit
        | _ -> ()
      in
      clear_graph ();
      display map player sun moon;
      synchronize ();
    done;
  with Exit -> close_graph (); exit (0)
