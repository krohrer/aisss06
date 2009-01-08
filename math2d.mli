module V2 :
sig
  type 'a t' = 'a * 'a
  type t = float t'

  val print : t -> unit

  val x : 'a t' -> 'a
  val y : 'a t' -> 'a
  
  val xx : 'a t' -> 'a t'
  val yy : 'a t' -> 'a t'
  val xy : 'a t' -> 'a t'
  val yx : 'a t' -> 'a t'
  
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
  val ortho     : t -> t
  val norm      : t -> float
  val normalize : t -> t
  
  val zero  : t
  val one   : t
  val ex    : t
  val ey    : t
  
  val sx    : float -> t
  val sy    : float -> t
end

(*----------------------------------------------------------------------------*)

module M2 :
sig
  type 'a row_t' = 'a V2.t'
  type 'a col_t' = 'a V2.t'
  type 'a t' = 'a col_t' row_t'
  type t = float t'

  val print : t -> unit
  
  val col1 : 'a t' -> 'a col_t'
  val col2 : 'a t' -> 'a col_t'
  val row1 : 'a t' -> 'a row_t'
  val row2 : 'a t' -> 'a row_t'

  val trans   : 'a t' -> 'a t'
  val col_map : ('a col_t' -> 'b) -> 'a t' -> 'b row_t'
  val row_map : ('a row_t' -> 'b) -> 'a t' -> 'b col_t'
  val map     : ('a -> 'b)        -> 'a t' -> 'b t'
  val fold    : ('a -> 'a -> 'a)  -> 'a t' -> 'a
  
  val neg   : t -> t
  val add   : t -> t -> t
  val sub   : t -> t -> t
  val mul1  : t -> float -> t
  val mul2  : t -> V2.t -> V2.t
  val mul   : t -> t -> t
  
  val det   : t -> float
  val inv   : t -> t
  val inv'  : det:float -> t -> t
  
  val one       : t
  val identity  : t
  
  val scale     : x:float -> y:float -> t
  val rotate    : angle:float -> t
end
