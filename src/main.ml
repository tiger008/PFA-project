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

let touche = function
  | FR -> ('z', 's', 'q', 'd', 'a', 'e')
  | _ -> ('w', 's', 'a', 'd', 'q', 'e')

let () =
  let ((x, y, a), sl) = read_lab (open_in argv.(1))  in
  let player = new_player (new_point x y) a win_w fov in
  let map = build_bsp (seglist sl []) in
  (*
    (* DEBUG *)
    Printf.printf "%s\n" (string_of_bsp map);
  *)
  open_graph (Printf.sprintf " %dx%d" win_w win_h);
  auto_synchronize false;
  try
    display map player;
    synchronize ();
    while true do
      let (fw, bw, left, right, rleft, rright) = touche (get_lang ()) in
      let s = Graphics.wait_next_event [Graphics.Key_pressed] in
      let _ = match s.key with
        | _ when s.key = fw -> move MFwd player map
        | _ when s.key = bw -> move MBwd player map
        | _ when s.key = left-> move MLeft player map
        | _ when s.key = right -> move MRight player map
        | _ when s.key = rleft -> rotate Left player
        | _ when s.key = rright -> rotate Right player
        | 'l' -> change_lang (get_lang ())
        | 'c' -> change_mode (get_mode ())
        | '\027' -> raise Exit
        | _ -> ()
      in
      clear_graph ();
      display map player;
      synchronize ();
    done;
  with Exit -> close_graph (); exit (0)
