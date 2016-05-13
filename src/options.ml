type tmode = TwoD | ThreeD
type tlang = FR | US
type ttime = Day | Night
type tperspective = FPS | RPG
                    
let usage = "usage: ./bsp file.lab"
let file = ref ""

let mode = ref TwoD
let lang = ref FR
let time = ref Day
let perspective = ref FPS
               
let win_w = ref 800
let win_h = ref 800

let fov = ref 60
let hov = ref 1

let step_dist = ref 10

let xmin = ref 1
let xmax = 9000.

let scale = ref 1
let minimap = ref false

let debug = ref false
let debug_bsp = ref false

let set_mode = function
  | "2D" -> mode := TwoD
  | "3D" -> mode := ThreeD
  | _ -> raise (Arg.Bad "2D or 3D only")
    
let change_mode = function
  | TwoD -> mode := ThreeD
  | _ -> mode := TwoD
    
let get_mode () = !mode
  
let set_lang = function
  | "FR" -> lang := FR
  | "US" -> lang := US
  | _ -> raise (Arg.Bad "FR or US only")
    
let change_lang = function
  | FR -> lang := US
  | _ -> lang := FR
    
let get_lang () = !lang

let increment_hov () =
  hov := !hov + 1

let decrement_hov () =
  hov := !hov - 1

let set_time = function
  | "Day" -> time := Day
  | "Night" -> time := Night
  |  _ -> ()

let change_time = function
  | Day -> time := Night
  | Night -> time := Day
  | _ -> ()
    
let get_time () = !time

let set_perspective = function
  | "FPS" -> perspective := FPS
  | "RPG" -> perspective := RPG
  | _ -> ()
                              
let change_perspective = function
  | FPS -> perspective := RPG
  | RPG -> perspective := FPS
                            
let get_perspective () = !perspective
  
let specs =
  [ "-mode", Arg.String set_mode, "<2D | 3D> 2D or 3D display";
    "-lang", Arg.String set_lang, " <FR | US> FR or US lang";
    "-fov", Arg.Set_int fov, " field of vision (angle de vision)";
    "-hov", Arg.Set_int hov, " height of vision (hauteur de vision)";
    "-dims", Arg.Tuple [Arg.Set_int win_w; Arg.Set_int win_h], 
    " set the dimensions of the graph";
    "-scale", Arg.Set_int scale, " scale of the 2D map";
    "-map", Arg.Set minimap, " set a minimap in the lower left corner";
    "-step", Arg.Set_int step_dist, " set the distance between two steps";
    "-xmin", Arg.Set_int xmin, " set minimum distance of display";
    "-debug", Arg.Set debug, " debugging 2D rendering";
    "-debugbsp", Arg.Set debug_bsp, " debugging bsp";
    "-time", Arg.String set_time, " <Day | Night> Day or Night display";
    "-persp", Arg.String set_perspective, " <FPS | RPS> FPS or RPS perspective"
  ]

let alspecs = Arg.align specs

let cin =
  let ofile = ref None in
  let set_file s =
    if Filename.check_suffix s ".lab" then ofile := Some s
    else raise (Arg.Bad "no .lab extension");
  in
  Arg.parse alspecs set_file usage;
  match !ofile with 
    | Some f -> file := f ; open_in f
    | None -> raise (Arg.Bad "no file provided")
  
let file = !file

let win_w = !win_w
let win_h = !win_h

let xmin = float !xmin

let ceiling_h = win_h / 4
let floor_h = 0
let wall_h = ceiling_h - floor_h

let fov = !fov
let get_hov () = !hov * ceiling_h / 2

let step_dist = float !step_dist

let scale = !scale
let minimap = !minimap

let debug = !debug
let debug_bsp = !debug_bsp
