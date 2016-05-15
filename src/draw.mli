val draw2D : Fsegment.t -> Player.t -> unit

val draw3D : Fsegment.t -> unit

val draw_player2D : int -> Player.t -> unit

val draw_player3D : Player.t -> unit

val draw_minimap : Bsp.t -> Player.t -> unit

val draw_sun : Sun.t -> Player.t -> int -> unit

val draw_moon : Moon.t -> Player.t -> int -> unit
