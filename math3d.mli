open Math2d

module V3 :
sig
  type 'a t' = 'a * 'a * 'a
  type t = float t'

  val print : t -> unit
  val of2 : ?z:float -> V2.t -> t
  val to2 : t -> V2.t

  val x : 'a t' -> 'a
  val y : 'a t' -> 'a
  val z : 'a t' -> 'a
  
  val xxx : 'a t' -> 'a t'
  val yyy : 'a t' -> 'a t'
  val zzz : 'a t' -> 'a t'
  val xyz : 'a t' -> 'a t'
  val xzy : 'a t' -> 'a t'
  val yxz : 'a t' -> 'a t'
  val yzx : 'a t' -> 'a t'
  val zxy : 'a t' -> 'a t'
  val zyx : 'a t' -> 'a t'
  
  val op    : ('a -> 'b -> 'c) -> 'a t' -> 'b t' -> 'c t'
  val op1   : ('a -> 'b -> 'c) -> 'a t' -> 'b    -> 'c t'
  val map   : ('a -> 'b)       -> 'a t' -> 'b t'
  val fold  : ('a -> 'a -> 'a) -> 'a t' -> 'a
  
  val neg   : t -> t
  val add   : t -> t -> t
  val sub   : t -> t -> t
  val mul   : t -> t -> t
  val div   : t -> t -> t
  val add1  : t -> float -> t
  val sub1  : t -> float -> t
  val mul1  : t -> float -> t
  val div1  : t -> float -> t
  val sum   : t -> float
  val prod  : t -> float
  
  val abs   : t -> t
  val min   : t -> t -> t
  val max   : t -> t -> t
  
  val dot       : t -> t -> float
  val cross     : t -> t -> t
  val norm      : t -> float
  val normalize : t -> t
  
  val zero  : t
  val one   : t
  val ex    : t
  val ey    : t
  val ez    : t
  
  val sx    : float -> t
  val sy    : float -> t
  val sz    : float -> t
end

(*----------------------------------------------------------------------------*)

module M3 :
sig
  type 'a row_t' = 'a V3.t'
  type 'a col_t' = 'a V3.t'
  type 'a t' = 'a col_t' row_t'
  type t = float t'

  val print : t -> unit
  
  val col1 : 'a t' -> 'a col_t'
  val col2 : 'a t' -> 'a col_t'
  val col3 : 'a t' -> 'a col_t'
  val row1 : 'a t' -> 'a row_t'
  val row2 : 'a t' -> 'a row_t'
  val row3 : 'a t' -> 'a row_t'

  val trans   : 'a t' -> 'a t'
  val col_map : ('a col_t' -> 'b) -> 'a t' -> 'b row_t'
  val row_map : ('a row_t' -> 'b) -> 'a t' -> 'b col_t'
  val map     : ('a -> 'b)        -> 'a t' -> 'b t'
  val fold    : ('a -> 'a -> 'a)  -> 'a t' -> 'a
  
  val neg   : t -> t
  val add   : t -> t -> t
  val sub   : t -> t -> t
  val mul1  : t -> float -> t
  val mul3  : t -> V3.t -> V3.t
  val mul   : t -> t -> t
  
  val det   : t -> float
  val inv   : t -> t
  val inv'  : det:float -> t -> t
  
  val one       : t
  val identity  : t
  
  val scale     : x:float -> y:float -> t
  val rotate_x  : angle:float -> t
  val rotate_y  : angle:float -> t
  val rotate_z  : angle:float -> t
  val translate : x:float -> y:float -> t
  
  val to_array : t -> float array array
end

(*----------------------------------------------------------------------------*)

module Qu :
sig
  type t = float * V3.t

  val print : t -> unit
  
  val real    : t -> float
  val unreal  : t -> V3.t

  val add : t -> t -> t    
  val sub : t -> t -> t
  val mul : t -> t -> t
  val mul1 : t -> float -> t
  
  val norm      : t -> float
  val normalize : t -> t
  val conj      : t -> t
  val inv       : t -> t
  
  val identity : t
  
  val to_matrix   : t -> M3.t
  val of_rotation : axis:V3.t -> angle:float -> t
end

(*----------------------------------------------------------------------------*)

module AT3 :
sig
  type transformation_t = M3.t * V3.t
  type t
  
  val print : t -> unit
  
  val create : transformation_t -> t
  
  val forward : t -> transformation_t
  val inverse : t -> transformation_t
  
  val transform_point  : t -> V3.t -> V3.t
  val transform_vector : t -> V3.t -> V3.t
  val transform_normal : t -> V3.t -> V3.t
  
  val identity : t
  
  val invert    : t -> t
  val concat    : t -> t -> t
  val scale     : V3.t -> t -> t
  val rotate_x  : angle:float -> t -> t
  val rotate_y  : angle:float -> t -> t
  val rotate_z  : angle:float -> t -> t
  val translate : V3.t -> t -> t
end