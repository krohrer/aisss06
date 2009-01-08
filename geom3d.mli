open Math3d

module FTest :
sig
  type cls = Negative | Epsilon | Positive
  type cls_space = NegSpace | PosSpace | EpsSpace | SpanSpace
  val eps : float
  
  val to_space : cls -> cls_space
  val concat : cls_space -> cls -> cls_space
  val concat_spaces : cls_space -> cls_space -> cls_space 

  val classify : float -> cls
  val classify_fold : cls_space -> float list -> cls_space
end

(*----------------------------------------------------------------------------*)

module Point :
sig
  type t = V3.t
  
  val transform : AT3.t -> t -> t
  
  val print : t -> unit
end

(*----------------------------------------------------------------------------*)

module Line :
sig
  type t = Point.t*Point.t
  
  val distance_to_point : t -> Point.t -> float
  
  val transform : AT3.t -> t -> t

  val print : t -> unit
end

(*----------------------------------------------------------------------------*)

module Plane :
sig
  type t = V3.t * float

  val create : a:Point.t -> n:V3.t -> t
  val create_from_points : Point.t -> Point.t -> Point.t -> t

  val normal : t -> V3.t
  val anchor : t -> Point.t
  val distance_to_origin : t -> float
  val distance_to_point : t -> Point.t -> float

  val classify_point : t -> Point.t -> FTest.cls
  val classify_points : t -> Point.t list -> FTest.cls_space
  val project_point : t -> Point.t -> Point.t
  val mirror_point : t -> Point.t -> Point.t
  
  val intersect2 : t -> t -> Line.t
  val intersect3 : t -> t -> t -> V3.t
  val intersect_line : t -> Line.t -> Point.t
  
  val transform : AT3.t -> t -> t

  val print : t -> unit
end

(*----------------------------------------------------------------------------*)

module Polygon :
sig
  type t = Point.t list 
  
  val flatten : Plane.t -> t -> t
  val split : Plane.t -> t -> (t*t)

  val create_slab_from_plane : ccw:bool -> dim:float -> Plane.t -> t
  
  val transform : AT3.t -> t -> t
  
  val print : t -> unit
end

(*----------------------------------------------------------------------------*)

module Polyhedron :
sig
  type t = Plane.t list
  
  val create_box : Point.t -> Point.t -> t
  
  val transform : AT3.t -> t -> t
  
  val print : t ->  unit
end

(*----------------------------------------------------------------------------*)

module AABB :
sig
  type t
  
  val create : center:Point.t -> extent:V3.t -> t
  val create_hull : corners:Point.t list -> t
  
  val min : t -> Point.t
  val max : t -> Point.t
  
  val center : t -> Point.t
  val extent : t -> V3.t
  
  val to_polyhedron : t -> Polyhedron.t
  
  val add_point : t -> Point.t -> t
  val add_points : t -> Point.t list -> t
  val union : t -> t -> t
  
  val classify_point : t -> Point.t -> FTest.cls

  val print : t -> unit
end
