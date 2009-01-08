open Math2d

module V3 =
struct
  type 'a t' = 'a * 'a * 'a
  type t = float t'

  let of2 ?(z=0.0) (x, y) = x, y, z
  let to2 (x, y, z) = x, y
  
  let x (e, _, _) = e
  let y (_, e, _) = e
  let z (_, _, e) = e
  
  let xxx (x, _, _) = x, x, x 
  let yyy (_, y, _) = y, y, y
  let zzz (_, _, z) = z, z, z
  let xyz (x, y, z) = x, y, z
  let xzy (x, y, z) = x, z, y
  let yxz (x, y, z) = y, x, z
  let yzx (x, y, z) = y, z, x
  let zxy (x, y, z) = z, x, y
  let zyx (x, y, z) = z, y, x
  
  let op op (ux, uy, uz) (vx, vy, vz) =
    op ux vx, op uy vy, op uz vz

  let op1 op (x,y,z) s =
    op x s, op y s, op z s

  let map f (x,y,z) = f x, f y, f z
  let fold f (x,y,z) = f (f x y) z
  
  let neg = map ( ~-. )
  let add = op ( +. )
  let sub = op ( -. )
  let mul = op ( *. )
  let div = op ( /. )
  let add1 = op1 ( +. )
  let sub1 = op1 ( -. )
  let mul1 = op1 ( *. )
  let div1 = op1 ( /. )
  let sum  = fold ( +. )
  let prod = fold ( *. )
  
  let abs = map abs_float
  let min = op min
  let max = op max
  
  let dot u v =
    sum (mul u v)
  let cross u v =
    sub
      (mul (yzx u) (zxy v))
      (mul (zxy u) (yzx v))
  
  let norm v =
    sqrt (dot v v)
  let normalize v =
    let invn = 1. /. norm v in mul1 v invn
  
  let zero  = 0., 0., 0.
  let one   = 1., 1., 1.
  let ex    = 1., 0., 0.
  let ey    = 0., 1., 0.
  let ez    = 0., 0., 1.

  let sx x = x , 0., 0.
  let sy y = 0., y , 0.
  let sz z = 0., 0., z

  let print (x,y,z) =
    Format.printf "@[(@ % 10.4g@ % 10.4g@ % 10.4g@ )@]" x y z
end

(*----------------------------------------------------------------------------*)

module M3 =
struct
  type 'a row_t' = 'a V3.t'
  type 'a col_t' = 'a V3.t'
  type 'a t' = 'a col_t' row_t'
  type t = float t'

  let col1 (c,_,_) = c
  let col2 (_,c,_) = c
  let col3 (_,_,c) = c
  let row1 m = V3.map V3.x m
  let row2 m = V3.map V3.y m
  let row3 m = V3.map V3.z m
  
  let trans m = row1 m, row2 m, row3 m
  let col_map = V3.map
  let row_map f m = V3.map f (trans m)
  let map f = V3.map (V3.map f)
  let fold f m = let f = V3.fold f in f (V3.map f m)
  
  let neg = V3.map V3.neg
  let add = V3.op V3.add
  let sub = V3.op V3.sub
  let mul1 m s = map (( *. ) s) m
  let mul3 m v = row_map (V3.dot v) m
  let mul m1 m2 = col_map (mul3 m1) m2
  
  let det ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) =
    let a = x1*.y2*.z3 -. x1*.y3*.z2 in
    let b = x2*.y1*.z3 -. x2*.y3*.z1 in
    let c = x3*.y1*.z2 -. x3*.y2*.z1 in
      a -. b +. c
      
  let d2' a b c d = a *. b -. c *. d

  let inv' ~det ((a11,a21,a31),(a12,a22,a32),(a13,a23,a33)) =
    let c1 = d2' a22 a33 a23 a32,  d2' a23 a31 a21 a33,  d2' a21 a32 a22 a31 in
    let c2 = d2' a13 a32 a12 a33,  d2' a11 a33 a13 a31,  d2' a12 a31 a11 a32 in
    let c3 = d2' a12 a23 a13 a22,  d2' a13 a21 a11 a23,  d2' a11 a22 a12 a21 in
       mul1 (c1, c2, c3) (1. /. det)

  let inv m =
    inv' ~det:(det m) m
  
  let one       = V3.one, V3.one, V3.one
  let identity  = V3.ex, V3.ey, V3.ez
  
  let scale ~x ~y =
    let c1 = x, 0., 0. in
    let c2 = 0., y, 0. in
      c1, c2, V3.ez
  
  let rotate_x ~angle =
    let c, s = cos angle, sin angle in
    let c2 = 0.,   c, s in
    let c3 = 0., -.s, c in
      V3.ex, c2, c3
      
  let rotate_y ~angle =
    let c, s = cos angle, sin angle in
    let c1 = c, 0., -.s in
    let c3 = s, 0.,   c in
      c1, V3.ey, c3

  let rotate_z ~angle =
    let c, s = cos angle, sin angle in
    let c1 =   c, s, 0. in
    let c2 = -.s, c, 0. in
      c1, c2, V3.ez

  let translate ~x ~y =
    V3.ex, V3.ey, (x,y,1.)
    
  let to_array ((a00, a10, a20), (a01, a11, a21), (a02, a12, a22)) =
    (* FIXME : what format do we have here?
     * assuming column-major and the same semantics as c arrays,
     * this should be correct. At least I hope so.
     *)
    [|
      [|a00; a10; a20; 0.0|];
      [|a01; a11; a21; 0.0|];
      [|a02; a12; a22; 0.0|];
      [|0.0; 0.0; 0.0; 1.0|];
    |]
    
  let print m =
    Format.open_vbox 2;
    Format.print_string "Math3d.M3:";
      Format.print_cut (); V3.print (row1 m);
      Format.print_cut (); V3.print (row2 m);
      Format.print_cut (); V3.print (row3 m);
    Format.close_box ()
end

(*----------------------------------------------------------------------------*)

module Qu =
struct
  type t = float * V3.t
  
  let real (w, _) = w
  let unreal (_, v) = v

  let add (w1, v1) (w2, v2) =
    w1 +. w2, V3.add v1 v2
    
  let sub (w1, v1) (w2, v2) =
    w1 -. w2, V3.sub v1 v2
  
  let mul (w1, v1) (w2, v2) =
    let w = w1 *. w2 -. V3.dot v1 v2 in
    let v = V3.add (V3.mul1 v1 w2) (V3.mul1 v2 w1) in
    let v = V3.add v (V3.cross v1 v2) in
      w, v

  let mul1 (w, v) s =
    w *. s, V3.mul1 v s
  
  let norm (w, v) =
    sqrt (w *. w +. V3.dot v v)
    
  let normalize q =
    let invn = 1./.norm q in
      mul1 q invn
      
  let conj (w, v) =
    w, V3.neg v

  let inv q =
    let invn = 1. /. norm q in
      mul1 (conj q) invn
      
  let identity = 1.0, V3.zero
  
  let to_matrix (w, v) =
    let xx, yy, zz = V3.mul v v in
    let xy, yz, xz = V3.mul v (V3.yzx v) in
    let xw, yw, zw = V3.mul1 v w in
    let col1 = 1. -. 2.*.(yy +. zz), 2.*.(xy +. zw), 2.*.(xz -. yw) in
    let col2 = 2.*.(xy -. zw), 1. -. 2.*.(xx +. zz), 2.*.(yz +. xw) in
    let col3 = 2.*.(xz +. yw), 2.*.(yz -. xw), 1. -. 2.*.(xx +. yy) in
      col1, col2, col3

  let of_rotation ~axis ~angle =
    let ha = angle /. 2. in
      normalize (cos ha, V3.mul1 (V3.normalize axis) (sin ha))
      
  let print (w, (x,y,z)) =
    Format.printf "@[(% .5g@ %+.5g*i@ %+.5g*j@ %+.5g*k@ )@]" w x y z
end

(*----------------------------------------------------------------------------*)

(* reversable affine transformation in R3 *)
module AT3 =
struct
  type transformation_t = M3.t * V3.t
  type t = { fwd : transformation_t ; inv : transformation_t Lazy.t }
  
  let forward at  = at.fwd
  let inverse at  = Lazy.force_val at.inv

  let identity =
    let tr = M3.identity, V3.zero in
      { fwd = tr; inv = Lazy.lazy_from_val tr }
  
  let transform' (m, t) v =
    V3.add (M3.mul3 m v) t

  let inv' (m, t) =
    let minv = M3.inv m in
    let tinv = M3.mul3 minv (V3.neg t) in
      minv, tinv
  
  let concat' (m1, t1) (m2, t2) =
    let tr = M3.mul m1 m2, transform' (m1, t1) t2 in
      { fwd = tr; inv = lazy (inv' tr) }
  
  let transform_point at p =
    transform' at.fwd p
    
  let transform_vector at v =
    let m, _ = at.fwd in
      M3.mul3 m v
    
  let transform_normal at n =
    let minv, _ = inverse at in
      M3.col_map (V3.dot n) minv

  let create tr =
    { fwd = tr; inv = lazy (inv' tr) }
  
  let invert at =
    { fwd = inverse at; inv = Lazy.lazy_from_val (forward at) }

  let concat at1 at2 =
    concat' at1.fwd at2.fwd
      
  let scale (sx, sy, sz) at =
    let m = (sx, 0., 0.), (0., sy, 0.), (0., 0., sz) in
      concat' (m, V3.zero) at.fwd
      
  let translate v at =
    concat' (M3.identity, v) at.fwd
  
  let rotate_x ~angle at =
    let m = M3.rotate_x ~angle:angle in
      concat' (m, V3.zero) at.fwd
  
  let rotate_y ~angle at =
    let m = M3.rotate_y ~angle:angle in
      concat' (m, V3.zero) at.fwd
      
  let rotate_z ~angle at =
    let m = M3.rotate_z ~angle:angle in
      concat' (m, V3.zero) at.fwd

  let print at =
    let m, t = forward at in
    let minv, tinv = inverse at in
    Format.open_vbox 2;
    Format.print_string "Math3d.AT3:";
      Format.print_cut ();
      Format.open_vbox 2;
      Format.print_string "forward:";
        Format.print_cut (); M3.print m;
        Format.print_cut (); V3.print t;
      Format.close_box ();
      Format.print_cut ();
      Format.open_vbox 2;
      Format.print_string "inverse:";
        Format.print_cut (); M3.print minv;
        Format.print_cut (); V3.print tinv;
      Format.close_box ();
    Format.close_box ()
end