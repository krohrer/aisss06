open Math3d

(*----------------------------------------------------------------------------*)

module FTest =
struct
  type cls = Negative | Epsilon | Positive
  type cls_space = NegSpace | PosSpace | EpsSpace | SpanSpace
  let eps = sqrt epsilon_float
  
  let classify f =
    if f > eps then
      Positive
    else if f < ~-.eps then
      Negative
    else
      Epsilon
      
  let to_space = function
  | Negative -> NegSpace
  | Epsilon  -> EpsSpace
  | Positive -> PosSpace  

  let concat_spaces c1 c2 =
    match c1, c2 with
    | SpanSpace, _
    | _, SpanSpace ->
        SpanSpace
    | EpsSpace, c
    | c, EpsSpace ->
        c
    | c1, c2 ->
        if c1 = c2 then c1 else SpanSpace
    
  let concat cspc c =
    concat_spaces cspc (to_space c)
      
  let classify_fold def clist =
    List.fold_left concat def (List.map classify clist)
end

(*----------------------------------------------------------------------------*)

module Point =
struct
  type t = V3.t
  
  let transform = AT3.transform_point
  
  let print = V3.print
end

(*----------------------------------------------------------------------------*)

module Line =
struct
  type t = Point.t * Point.t

  let distance_to_point (sl, el) p =
    let vl = V3.sub el sl in
    let vp = V3.sub el p in
    let area = V3.norm (V3.cross vl vp) in
      area /. (V3.norm vl)
      
  let transform at (sl, el) =
    Point.transform at sl, Point.transform at el
  
  let print (sl, el) =
    Format.open_vbox 2;
    Format.print_string "Geom3d.Line:";
      Format.print_cut (); Point.print sl;
      Format.print_cut (); Point.print el;
    Format.close_box ()
end

(*----------------------------------------------------------------------------*)

module Plane =
struct
  type t = V3.t * float
  (* (n,d): n dot (x,y,z) + d == 0, |n| = 1. *)
  
  let create ~a ~n =
    let n = V3.normalize n in
      (n, ~-.(V3.dot n a))
      
  let create_from_points p1 p2 p3 =
    let n = V3.cross (V3.sub p2 p1) (V3.sub p3 p1) in
      create ~a:p1 ~n:n

  let normal (n, _) = n
  let anchor (n, d) = V3.mul1 n ~-.d
  let distance_to_origin (_, d) = d
  let distance_to_point (n, d) p = V3.dot n p +. d

  let classify_point h p =
    FTest.classify (distance_to_point h p)
    
  let classify_points h ps =
    let distances = List.map (distance_to_point h) ps in
      FTest.classify_fold FTest.EpsSpace distances
    
  let project_point (n, d) p =
    let dp = distance_to_point (n, d) p in
      V3.sub p (V3.mul1 n dp)
      
  let mirror_point (n, d) p =
    let dp = distance_to_point (n, d) p in
      V3.sub p (V3.mul1 n (2. *. dp))
  
  let intersect3 (n1, d1) (n2, d2) (n3, d3) =
    let ninv = M3.inv (M3.trans (n1, n2, n3)) in
    let d = V3.neg (d1, d2, d3) in
      M3.mul3 ninv d
      
  let intersect2 (n1, d1) (n2, d2) =
    let n3 = V3.cross n1 n2 in
    let o = intersect3 (n1, d1) (n2, d2) (n3, 0.) in
      (o, V3.add o n3)
      
  let intersect_line h (p1, p2) =
    let d1 = distance_to_point h p1 in
    let d2 = distance_to_point h p2 in
      (* (d1*p1 - d2*p2) / (d1-d2) *)
      V3.div1 (V3.sub (V3.mul1 p1 d2) (V3.mul1 p2 d1)) (d2 -. d1)

  let transform at p =
    let n = AT3.transform_normal at (normal p) in
    let a = AT3.transform_point  at (anchor p) in
      create ~n:n ~a:a

  let print (n, d) =
    Format.open_vbox 2;
    Format.print_string "Geom3d.Plane:";
      Format.print_cut (); Format.print_string "normal   = ";
      V3.print n;
      Format.print_cut (); Format.print_string "distance = ";
      Format.print_float d;
    Format.close_box ()
end

(*----------------------------------------------------------------------------*)

module Polygon =
struct
  type t = Point.t list
  
  let flatten plane poly =
    List.map (Plane.project_point plane) poly
  
  (* iterate over edges and decide on each vertex, split as neccessary *)
  (* caution : reverses the order of the vertices *)
  let rec splitter_rev' hs (prev_vtx,prev_cls) (vneg,vpos) = function
  | [] -> (vneg,vpos)
  | vtx :: rest -> begin
      let cls = FTest.classify (Plane.distance_to_point hs vtx) in
      match prev_cls, cls with
      | FTest.Positive, FTest.Negative ->
          let vsplit = Plane.intersect_line hs (prev_vtx, vtx) in
          (* FIXMAY : make sure point is on plane *)
          (* let vsplit = Plane.project_point hs vsplit in *)
          let vneg = vtx :: vsplit :: vneg in
          let vpos =        vsplit :: vpos in
            splitter_rev' hs (vtx, cls) (vneg, vpos) rest
      | FTest.Negative, FTest.Positive ->
          let vsplit = Plane.intersect_line hs (prev_vtx, vtx) in
          (* FIXMAY : make sure point is on plane *)
          (* let vsplit = Plane.project_point hs vsplit in *)
          let vneg =        vsplit :: vneg in          
          let vpos = vtx :: vsplit :: vpos in
            splitter_rev' hs (vtx, cls) (vneg, vpos) rest
      | _, FTest.Epsilon ->
          let vneg = vtx :: vneg in
          let vpos = vtx :: vpos in
            splitter_rev' hs (vtx, cls) (vneg, vpos) rest
      | _, FTest.Negative ->
          let vneg = vtx :: vneg in
            splitter_rev' hs (vtx, cls) (vneg, vpos) rest
      | _, FTest.Positive ->
          let vpos = vtx :: vpos in
            splitter_rev' hs (vtx, cls) (vneg, vpos) rest
    end
          
  let split hs = function
  | [] ->
      ([],[])
  | verts ->
      let first_vtx = List.hd verts in
      let first_cls = FTest.classify (Plane.distance_to_point hs first_vtx) in
        splitter_rev' hs (first_vtx,first_cls) ([],[]) (List.rev verts)

  let choose_swizzle' (pm, ps) (m, s) =
    if m > pm then (m, s) else (pm, ps)

  let create_slab_from_plane ~ccw ~dim plane =
    (* proto-slab *)
    let points = [1., 1., 0.; -1., 1., 0.; -1., -1., 0.; 1., -1., 0.] in
    (* scale up proto-slab *)
    let points = List.map (V3.mul (V3.mul1 V3.one dim)) points in
    (* align proto-slab with major axis *)
    let (x, y, z), n = plane in
    let _, swizzle =
      List.fold_left choose_swizzle' (0.0, V3.xyz)
      [x, V3.zxy; -.x, V3.zyx; y, V3.yzx; -.y, V3.xzy; z, V3.xyz; -.z, V3.yxz]
    in
    let points = List.map swizzle points in
    (* drop prot-slab onto plane *)
    let project p = Plane.intersect_line plane (p, V3.add p (swizzle V3.ez)) in
    let points = List.map project points in
    (* cap slab at both ends to fit completely into universe *)
    let neg_cap = (swizzle (V3.neg V3.ez), -.dim) in
    let pos_cap = (swizzle         V3.ez , -.dim) in
    let points, _ = split neg_cap points in
    let points, _ = split pos_cap points in
    if ccw then
      points
        else
      List.rev points

  let transform at =
    List.map (Point.transform at)

  let print ps =
    Format.open_vbox 2;
    Format.print_string "Geom3d.Polygon:";
      List.iter (fun p -> Format.print_cut (); Point.print p) ps;
    Format.close_box ()
end

(*----------------------------------------------------------------------------*)

module Polyhedron =
struct
  type t = Plane.t list

  let create_box p1 p2 =
    let bmin = V3.op min p1 p2 in
    let bmax = V3.op max p1 p2 in
    [
      Plane.create ~a:bmin ~n:(V3.neg V3.ex);
      Plane.create ~a:bmin ~n:(V3.neg V3.ey);
      Plane.create ~a:bmin ~n:(V3.neg V3.ez);
      Plane.create ~a:bmax ~n:V3.ex;
      Plane.create ~a:bmax ~n:V3.ey;
      Plane.create ~a:bmax ~n:V3.ez;
    ]

  let transform at =
    List.map (Plane.transform at)
  
  let print ph =
    Format.open_vbox 2;
    Format.print_string "Geom3d.Polyhedron:";
      List.iter (fun p -> Format.print_cut (); Plane.print p) ph;
    Format.close_box ()
end

(*----------------------------------------------------------------------------*)

module AABB =
struct
  type t = Point.t * Point.t

  let add_point (bmin, bmax) p =
    (V3.op min bmin p, V3.op max bmax p)

  let add_points box points =
    List.fold_left add_point box points

  let create_hull ~corners =
    match corners with
      [] -> failwith "AABB.create_hull ~corners:[]"
    | p::rest -> add_points (p,p) rest

  let create ~center ~extent =
    let exth = V3.mul1 extent 0.5 in
      create_hull [V3.sub center exth; V3.add center exth]
      
  let min (bmin, _) = bmin
  let max (_, bmax) = bmax
  
  let center (m1, m2) = V3.mul1 (V3.add m1 m2) 0.5
  let extent (m1, m2) = V3.sub m2 m1
  
  let to_polyhedron (bmin, bmax) =
    Polyhedron.create_box bmin bmax

  let union box1 box2 =
    add_points box1 [fst box2; snd box2]
    
  let classify_point (bmin, bmax) p = FTest.Negative
  
  let print (bmin, bmax) =
    Format.open_vbox 2;
    Format.print_string "Geom3d.AABB:";
      Format.print_cut (); Format.print_string "min = ";
      Point.print bmin;
      Format.print_cut (); Format.print_string "max = ";
      Point.print bmax;
    Format.close_box ()
end
