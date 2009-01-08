module V2 =
struct
  type 'a t' = 'a * 'a
  type t = float t'

  let x (e, _) = e
  let y (_, e) = e
  
  let xx (x, _) = x, x 
  let yy (_, y) = y, y
  let xy (x, y) = x, y
  let yx (x, y) = y, x
  
  let op op (ux, uy) (vx, vy) =
    op ux vx, op uy vy

  let op1 op (x,y) s =
    op x s, op y s

  let map f (x,y) = f x, f y
  let fold f (x,y) = f x y
  
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
  let ortho (x, y) = (-.y, x)
  
  let norm v =
    sqrt (dot v v)
  let normalize v =
    let invn = 1. /. norm v in mul1 v invn
  
  let zero  = 0., 0.
  let one   = 1., 1.
  let ex    = 1., 0.
  let ey    = 0., 1.

  let sx x = x , 0.
  let sy y = 0., y

  let print (x,y) =
    Format.printf "@[(@ % 10.4g@ % 10.4g@ )@]" x y
end

(*----------------------------------------------------------------------------*)

module M2 =
struct
  type 'a row_t' = 'a V2.t'
  type 'a col_t' = 'a V2.t'
  type 'a t' = 'a col_t' row_t'
  type t = float t'

  let col1 (c,_) = c
  let col2 (_,c) = c
  let row1 m = V2.map V2.x m
  let row2 m = V2.map V2.y m
  
  let trans m = row1 m, row2 m
  let col_map = V2.map
  let row_map f m = V2.map f (trans m)
  let map f = V2.map (V2.map f)
  let fold f m = let f = V2.fold f in f (V2.map f m)
  
  let neg = V2.map V2.neg
  let add = V2.op V2.add
  let sub = V2.op V2.sub
  let mul1 m s = map (( *. ) s) m
  let mul2 m v = row_map (V2.dot v) m
  let mul m1 m2 = col_map (mul2 m1) m2
  
  let det ((x1,y1),(x2,y2)) =
    x1 *. y2 -. x2 *. y1
      
  let inv' ~det ((a11,a21),(a12,a22)) =
       mul1 ((a22,-.a21),(-.a21,a11)) (1. /. det)

  let inv m =
    inv' ~det:(det m) m
  
  let one       = V2.one, V2.one
  let identity  = V2.ex, V2.ey
  
  let scale ~x ~y =
    let c1 = x, 0. in
    let c2 = 0., y in
      c1, c2

  let rotate ~angle =
    let c, s = cos angle, sin angle in
    let c1 =   c, s in
    let c2 = -.s, c in
      c1, c2

  let print m =
    Format.open_vbox 2;
    Format.print_string "Math2d.M2:";
      Format.print_cut (); V2.print (row1 m);
      Format.print_cut (); V2.print (row2 m);
    Format.close_box ()
end
