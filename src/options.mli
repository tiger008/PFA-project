type tmode = TwoD | ThreeD
type tlang = FR | US
type ttime = Day | Night
type tperspective = FPS | RPG
                    
val change_mode : tmode -> unit
val get_mode : unit -> tmode

val change_lang : tlang -> unit
val get_lang : unit -> tlang

val change_time : ttime -> unit
val get_time : unit -> ttime

val change_perspective : tperspective -> unit
val get_perspective : unit -> tperspective

val increment_hov : unit -> unit
val decrement_hov : unit -> unit

val increment_rs : unit -> unit
val decrement_rs : unit -> unit
                         
val cin : in_channel

val win_w : int
val win_h : int

val ceiling_h : int
val floor_h : int
val wall_h : int

val fov : int
val get_hov : unit -> int
val get_rs : unit -> int
            
val step_dist : float

val xmin : float
val xmax : float

val scale : int
val minimap : bool
 
val debug : bool
val debug_bsp : bool
