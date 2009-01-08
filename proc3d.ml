open Math3d
open Geom3d
open BspTree3d

type matter = BspTree3d.t
type material = BspTree3d.material_t

(*- monad stuff --------------------------------------------------------------*)

type state = { 
  dim       : float;
  mat       : material;
  lod       : int;
  transform : AT3.t;
  rnd       : Random.State.t array
}
type 'a mproc = state -> ('a*state)

(* bind *)
let bind m f =
  fun s ->
    let a, s = m s in
      f a s

(* return  *)
let ret a =
  fun s -> a, s

(* convenience *)
let ( >>= ) = bind
let ( >>> ) ma mb = perform a <-- ma; mb
let ( --> ) a mab = ret a >>= mab

(* monadic functions *)
let mseq mlist =
  let mcons p q =
    perform
      x <-- p;
      y <-- q;
      ret (x :: y)
  in
    List.fold_right mcons mlist (ret [])

let rec mrep n maa a =
  if n = 0 then
    ret a
  else
    perform
      a <-- maa a;
      mrep (n-1) maa a
    
let mmap f al = mseq (List.map f al)

let rec mfold f a = function
| [] ->
    ret a
| x :: xs ->
    perform
      y <-- f a x;
      mfold f y xs

let mfold' f l a = mfold f a l 

let mwhen   p m = if p then m else ret ()
let munless p m = mwhen (not p) m
let mwhen'    p ma a = if p then ma a else ret a
let munless'  p ma a = mwhen' (not p) ma a
let mif     p mt mf = if p then mt else mf
let mlift   f ma = perform a' <-- ma; ret (f a')
let mlift2  f ma mb = perform a' <-- ma; b' <-- mb; ret (f a' b')

(*- proc geometry stuff ------------------------------------------------------*)

let emerge ~dim ~lod ~transform ~seed mp =
  let s = {
      dim = dim;
      lod = lod;
      mat = BspTree3d.mat_all;
      transform = transform;
      rnd = Array.init 10 (fun _ -> Random.State.make seed)
  } in
  let m, _ = mp s in
    m

(* internal helpers *)
let gets s = s, s
let sets s = fun _ -> (), s

(* save state *)
let call m =
  perform
    s <-- gets;
    (*let s = { s with rnd = Random.State.copy s.rnd } in *)
    r <-- m;
    sets s;
    ret r

let call1 maa a = call (maa a)

(* bridge a 'a mproc *)
let pass ma b = ma >>> ret b

let detail lvl more less =
  perform
    s <-- gets;
    r <--
      if s.lod + lvl <= 0 then
        sets {s with lod = s.lod + lvl(*; rnd = Random.State.copy s.rnd*) } >>> more
      else
        less;
    sets s ;
    ret r
    
let lod lvl mm m =
  perform
    detail lvl (m --> mm) (ret m)

let void = ret (BspTree3d.cell BspTree3d.mat_none)

(* box with corners (-1.,-1.,-1.)x(1.,1.,1.) *)
let ubox =
  perform
    s <-- gets;
    let poly = Polyhedron.create_box (V3.neg V3.one) V3.one in
    let poly = Polyhedron.transform s.transform poly in
    ret (BspTree3d.create_from_polyhedron ~dim:s.dim ~mat:s.mat poly)

let box p1 p2 =
  perform
    s <-- gets;
    let poly = Polyhedron.create_box p1 p2 in
    let poly = Polyhedron.transform s.transform poly in
    ret (BspTree3d.create_from_polyhedron ~dim:s.dim ~mat:s.mat poly)

(* halfspace *)
let halfp p =
  perform
    s <-- gets;
    let poly = Polyhedron.transform s.transform [p] in
    ret (BspTree3d.create_from_polyhedron ~dim:s.dim ~mat:s.mat poly)

let half a n =
  let p = Plane.create ~a:a ~n:n in
    halfp p

let half3 p1 p2 p3 =
  let p = Plane.create_from_points p1 p2 p3 in
    halfp p

let ( ~~~ ) = BspTree3d.complement
let ( --- ) = BspTree3d.difference
let ( +++ ) = BspTree3d.union
let ( *** ) = BspTree3d.intersection
let ( %%% ) = BspTree3d.sym_difference

let mat_all = BspTree3d.mat_all
let mat_none = BspTree3d.mat_none
let unity = BspTree3d.cell

let unite     w m = mlift (( +++ ) w) m
let diff      w m = mlift (( --- ) w) m
let symdiff   w m = mlift (( %%% ) w) m
let intersect w m = mlift (( *** ) w) m
let unite'      m w = unite w m
let diff'       m w = diff w m
let symdiff'    m w = symdiff w m
let intersect'  m w = intersect w m
let lunite      l m w = lod l (unite' m) w
let ldiff       l m w = lod l (diff' m) w
let lsymdiff    l m w = lod l (symdiff' m) w
let lintersect  l m w = lod l (intersect' m) w

let ( >>++ ) m mm = m >>= unite' mm
let ( >>-- ) m mm = m >>= diff' mm
let ( >>** ) m mm = m >>= intersect' mm
let ( >>%% ) m mm = m >>= symdiff' mm

(* random number generation *)
let rnds s = s.rnd.(~-(s.lod))
let rnd l h =
  perform
    s <-- gets;
    ret (Random.State.float (rnds s) (h -. l) +. l)
let rndi l h =
  perform
    s <-- gets;
    ret (Random.State.int (rnds s) (h - l) + l)
let rnd3 (lx,ly,lz) (hx,hy,hz) =
  perform
    rx <-- rnd lx hx;
    ry <-- rnd ly hy;
    rz <-- rnd lz hz;
    ret (rx, ry, rz)

(* transformation *)
let transform at =
  perform s <-- gets; sets { s with transform = AT3.concat s.transform at }
let translate vt =
  perform transform (AT3.translate vt AT3.identity)
let scale vs =
  perform transform (AT3.scale vs AT3.identity)
let rotate_x angle =
  perform transform (AT3.rotate_x ~angle:angle AT3.identity)
let rotate_y angle =
  perform transform (AT3.rotate_y ~angle:angle AT3.identity)
let rotate_z angle =
  perform transform (AT3.rotate_z ~angle:angle AT3.identity)

let pi = 2.0 *. acos 0.0
let deg2rad d = d *. pi /. 180.0

let tfm t   = perform transform t
let tlx t   = perform translate (t,0.0,0.0)
let tly t   = perform translate (0.0,t,0.0)
let tlz t   = perform translate (0.0,0.0,t)
let tl3 xyz = perform translate xyz
let scx s   = perform scale (s,1.0,1.0)
let scy s   = perform scale (1.0,s,1.0)
let scz s   = perform scale (1.0,1.0,s)
let sc1 s   = perform scale (s,s,s)
let sc3 xyz = perform scale xyz
let rox a   = perform rotate_x (deg2rad a)
let roy a   = perform rotate_y (deg2rad a)
let roz a   = perform rotate_z (deg2rad a)
let mat m   = perform s <-- gets; sets { s with mat = m }

(* sophisticated geometry *)
let usphere =
  perform
    ubox >>= lod 1 (mfold' intersect [
        call (rox 45.0 >>> ubox);
        call (roy 45.0 >>> ubox);
        call (roz 45.0 >>> ubox)
    ])
    
let sphere p r =
  call (tl3 p >>> sc1 r >>> usphere)
  
let ucylinder =
  ubox >>= lintersect 1 (roz 45.0 >>> ubox)
      