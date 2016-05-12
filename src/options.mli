type tmode = TwoD | ThreeD
type tlang = FR | US

val change_mode : tmode -> unit
val get_mode : unit -> tmode

val cin : in_channel

val win_w : int
val win_h : int

val ceiling_h : int
val floor_h : int
val wall_h : int

val fov : int
val hov : int

val step_dist : float

val xmin : float
val xmax : float

val scale : int
val minimap : bool
 
val debug : bool
val debug_bsp : bool

val change_lang : tlang -> unit
val get_lang : unit -> tlang
