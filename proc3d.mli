open Math3d
open Geom3d
open BspTree3d

type matter = BspTree3d.t
type material = BspTree3d.material_t

(*- monad stuff --------------------------------------------------------------*)

type state
type 'a mproc = state -> ('a*state)

(* bind *)
val bind    : 'a mproc -> ('a -> 'b mproc) -> 'b mproc
val ( >>= ) : 'a mproc -> ('a -> 'b mproc) -> 'b mproc
val ( >>> ) : 'a mproc -> 'b mproc -> 'b mproc
val ( --> ) : 'a -> ('a -> 'b mproc) -> 'b mproc

(* like return in haskell *)
val ret : 'a -> 'a mproc

(* shamelessly taken from haskell, added some more like mif and mrep *)
val mseq    : ('a mproc) list -> ('a list) mproc
val mrep    : int -> ('a -> 'a mproc) -> 'a -> 'a mproc
val mmap    : ('a -> 'b mproc) -> 'a list -> ('b list) mproc
val mfold   : ('a -> 'b -> 'a mproc) -> 'a -> 'b list -> 'a mproc
val mfold'  : ('a -> 'b -> 'a mproc) -> 'b list -> ('a -> 'a mproc)

val mwhen   : bool -> unit mproc -> unit mproc
val munless : bool -> unit mproc -> unit mproc
val mwhen'    : bool -> ('a -> 'a mproc) -> 'a -> 'a mproc
val munless'  : bool -> ('a -> 'a mproc) -> 'a -> 'a mproc
val mif     : bool -> 'a mproc -> 'a mproc -> 'a mproc

val mlift   : ('a -> 'b) -> ('a mproc -> 'b mproc)
val mlift2  : ('a -> 'b -> 'c) -> ('a mproc -> 'b mproc -> 'c mproc)

(*- proc geometry stuff ------------------------------------------------------*)

val emerge : dim:float -> lod:int -> transform:AT3.t -> seed:int array -> matter mproc -> matter

(* save state *)
val call : 'a mproc -> 'a mproc
val call1 : ('a -> 'b mproc) -> 'a -> 'b mproc

val pass   : 'a mproc -> ('b -> 'b mproc)

val detail : int -> 'a mproc -> 'a mproc -> 'a mproc
val lod : int -> (matter -> matter mproc) -> matter -> matter mproc

val void : matter mproc
val ubox  : matter mproc
val box   : Point.t -> Point.t -> matter mproc
val usphere   : matter mproc
val sphere    : Point.t -> float -> matter mproc
val ucylinder : matter mproc
val halfp : Plane.t -> matter mproc
val half  : Point.t -> V3.t -> matter mproc
val half3 : Point.t -> Point.t -> Point.t -> matter mproc

val ( ~~~ ) : matter -> matter
val ( +++ ) : matter -> matter -> matter
val ( --- ) : matter -> matter -> matter
val ( *** ) : matter -> matter -> matter
val ( %%% ) : matter -> matter -> matter

val ( >>++ ) : matter mproc -> matter mproc -> matter mproc
val ( >>-- ) : matter mproc -> matter mproc -> matter mproc
val ( >>** ) : matter mproc -> matter mproc -> matter mproc
val ( >>%% ) : matter mproc -> matter mproc -> matter mproc

val mat_all : material
val mat_none : material
val unity : material -> matter

val unite     : matter -> matter mproc -> matter mproc
val diff      : matter -> matter mproc -> matter mproc
val symdiff   : matter -> matter mproc -> matter mproc
val intersect : matter -> matter mproc -> matter mproc
val unite'      : matter mproc -> matter -> matter mproc
val diff'       : matter mproc -> matter -> matter mproc
val symdiff'    : matter mproc -> matter -> matter mproc
val intersect'  : matter mproc -> matter -> matter mproc
val lunite      : int -> matter mproc -> matter -> matter mproc
val ldiff       : int -> matter mproc -> matter -> matter mproc
val lsymdiff    : int -> matter mproc -> matter -> matter mproc
val lintersect  : int -> matter mproc -> matter -> matter mproc

val rnd   : float -> float -> float mproc
val rndi  : int -> int -> int mproc
val rnd3  : Point.t -> Point.t -> Point.t mproc

val mat : material -> unit mproc
val tfm : AT3.t -> unit mproc
val tlx : float -> unit mproc
val tly : float -> unit mproc
val tlz : float -> unit mproc
val tl3 : V3.t  -> unit mproc
val scx : float -> unit mproc
val scy : float -> unit mproc
val scz : float -> unit mproc
val sc1 : float -> unit mproc
val sc3 : V3.t  -> unit mproc
val rox : float -> unit mproc
val roy : float -> unit mproc
val roz : float -> unit mproc
