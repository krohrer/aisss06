open Math3d
open Geom3d

type material_t = int
type op = Union | Intersection | Difference | InvDifference | SymDifference

type t
type face_t = Polygon.t * material_t * material_t
type faces_t

type draw_fn = Plane.t -> face_t -> unit
type filter_fn = material_t -> material_t -> bool

val empty : t
val cell : material_t -> t
val mat_all : material_t
val mat_none : material_t

val create_from_plane :
  dim:float -> mneg:material_t -> mpos:material_t -> Plane.t -> t
val create_from_polyhedron :
  dim:float -> mat:material_t-> Polyhedron.t -> t

val merge : op -> t -> t -> t

val condense    : t -> t
val complement  : t -> t

val union           : t -> t -> t
val intersection    : t -> t -> t
val difference      : t -> t -> t
val sym_difference  : t -> t -> t

val to_faces   : ?filterb:filter_fn -> ?filterf:filter_fn -> t -> faces_t
val draw_none  : draw_fn
val draw_tree  : draw:(Plane.t -> Polygon.t -> unit) -> eye:V3.t -> t -> unit
val draw_faces : ?drawb:draw_fn -> ?drawf:draw_fn -> eye:V3.t -> faces_t -> unit
