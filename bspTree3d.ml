open Math3d
open Geom3d
open ExtList

(* Each bit indicated the absence/presence of a material
 * thus there are 30 pure materials on a 32-bit machine
 * but there are also combinations of those 30 pure materials
 *
 * Merging materials boils down to simple bit-ops:
 *   Union -> m1 lor m2
 *   Intersection -> m1 land m2
 *   Difference -> m1 land (lnot m2)
 *   InvDifference -> (lnot m1) land m2
 *   SymDifference -> m1 xor m2
 *)
type material_t = int
type op = Union | Intersection | Difference | InvDifference | SymDifference

type t =
| Cell of material_t
| Node of Plane.t * Polygon.t * t * t

type face_t = Polygon.t * material_t * material_t
type faces_t =
| DCell
| DNode of Plane.t * face_t list * face_t list * faces_t * faces_t

type draw_fn = Plane.t -> face_t -> unit
type filter_fn = material_t -> material_t -> bool

let mat_none = 0
let mat_all = max_int
let empty = Cell mat_none
let cell m = Cell m

(* condense:
 * consolidate cells with the same attribute, apply postfix to each node
 *)
let rec condense = function
| Node (h, p, neg, pos) -> (
    let neg = condense neg in
    let pos = condense pos in
    match neg, pos with
    | Cell m1, Cell m2 ->
        if m1 = m2 then Cell m1 else Node (h, p, neg, pos)
    | _ ->
        Node (h, p, neg, pos)
  )
| Cell m -> Cell m

(* complement:
 *)
let rec complement = function
| Cell m ->
    Cell (lnot m)
| Node (h, p, neg, pos) ->
    Node (h, p, complement neg, complement pos)

let switch_op = function
| Union -> Union
| Intersection -> Intersection
| Difference -> InvDifference
| InvDifference -> Difference
| SymDifference -> SymDifference

(* merge_?_with_?:
 * transform tree T? according to operation OP and cell attribute A
 * we have to differentiate between two cases because the difference
 * operator is not symmetric. which sux.
 * Not anymore, with InvDifference that is.
 *)
let rec merge_cell_with_tree op m1 t2 =
  match t2 with
  | Cell m2 ->
      begin
        match op with
        | Union         -> Cell (m1 lor m2)
        | Intersection  -> Cell (m1 land m2)
        | Difference    -> Cell (m1 land (lnot m2))
        | InvDifference -> Cell ((lnot m1) land m2)
        | SymDifference -> Cell (m1 lxor m2)
      end
  (* FIXME : is this correct ? *)
  | Node (h, p, neg, pos) ->
      if m1 = mat_none then
        match op with
        | Union         -> t2
        | Intersection  -> Cell m1
        | Difference    -> Cell m1
        | InvDifference -> t2
        | SymDifference -> t2
      else if m1 = mat_all then
        match op with
        | Union         -> Cell m1
        | Intersection  -> t2
        | Difference    -> complement t2
        | InvDifference -> Cell mat_none
        | SymDifference -> complement t2
      else
        let neg = merge_cell_with_tree op m1 neg in
        let pos = merge_cell_with_tree op m1 pos in
          Node (h, p, neg, pos)
  
let merge_tree_with_cell op t1 a =
  merge_cell_with_tree (switch_op op) a t1

(* partition:
 * split every node and cell by P to receive tree that contains P
 *)
let rec partition hp1 shp1 = function
| Cell m ->
    Cell m, Cell m
| Node (hp2, shp2, neg, pos) ->
  begin
    let shp1vs2 = Plane.classify_points hp2 shp1 in
    let shp2vs1 = Plane.classify_points hp1 shp2 in
    match shp1vs2, shp2vs1 with

    (* Neg, _ -> neg gets partitioned *)
    (* Pos, _ -> pos gets partitioned *)
    (* _, Neg -> new node on negative side *)
    (* _, Pos -> new node on positive side *)

    | FTest.NegSpace, FTest.NegSpace ->
        let neg_in_neg, neg_in_pos = partition hp1 shp1 neg in
          Node (hp2, shp2, neg_in_neg, pos), neg_in_pos
          
    | FTest.NegSpace, FTest.PosSpace ->
        let neg_in_neg, neg_in_pos = partition hp1 shp1 neg in
          neg_in_neg, Node (hp2, shp2, neg_in_pos, pos)
          
    | FTest.PosSpace, FTest.NegSpace ->
        let pos_in_neg, pos_in_pos = partition hp1 shp1 pos in
          Node (hp2, shp2, neg, pos_in_neg), pos_in_pos 
          
    | FTest.PosSpace, FTest.PosSpace ->
        let pos_in_neg, pos_in_pos = partition hp1 shp1 pos in
            pos_in_neg, Node (hp2, shp2, neg, pos_in_pos)

    | FTest.EpsSpace, FTest.EpsSpace ->
        let n1, n2 = Plane.normal hp1, Plane.normal hp2 in
        if V3.dot n1 n2 > 0.0 then (* parallel *)
          neg, pos
        else (* anti-parallel *)
          pos, neg

    | FTest.SpanSpace, FTest.SpanSpace ->
        let shp1_in, shp1_ip = Polygon.split hp2 shp1 in
        let neg_in, neg_ip = partition hp1 shp1_in neg in
        let pos_in, pos_ip = partition hp1 shp1_ip pos in
        let shp2_in, shp2_ip = Polygon.split hp1 shp2 in
          Node (hp2, shp2_in, neg_in, pos_in),
          Node (hp2, shp2_ip, neg_ip, pos_ip)

    | FTest.EpsSpace, _
    | _, FTest.EpsSpace
    | FTest.SpanSpace, _
    | _, FTest.SpanSpace ->
        failwith "Bspt3d.classify_partitions : numerical errors"
  end
   
(* merge:
 * merge two trees according to operation OP
 *)
let rec merge op t1 t2 = 
  match t1, t2 with
  | Cell a, t ->
      condense (merge_cell_with_tree op a t)
  | t, Cell a ->
      condense (merge_tree_with_cell op t a)
  | Node (h, p, neg, pos), t2 ->
      let t2_neg, t2_pos = partition h p t2 in
      let t2_neg = condense t2_neg in
      let t2_pos = condense t2_pos in
      let neg = merge op neg t2_neg in
      let pos = merge op pos t2_pos in
	Node (h, p, neg, pos)
 
(* shortcuts *)
let union t1 t2           = condense (merge Union t1 t2)
let intersection t1 t2    = condense (merge Intersection t1 t2)
let difference t1 t2      = condense (merge Difference t1 t2)
let inv_difference t1 t2  = condense (merge InvDifference t1 t2)
let sym_difference t1 t2  = condense (merge SymDifference t1 t2)

(* draw_raw:
 * apply drawing function to all shps
 * EYEVEC is used to draw back to front
 *)
let rec draw_tree ~draw ~eye = function
| Cell _ -> ()
| Node (hp, shp, neg, pos) ->
    if Plane.distance_to_point hp eye < 0.0 then begin
      (* pos is facing away from viewer *)
      draw_tree ~draw:draw ~eye:eye pos;
      draw hp shp;
      draw_tree ~draw:draw ~eye:eye neg
    end else begin
      (* neg "" *)
      draw_tree ~draw:draw ~eye:eye neg;
      draw hp shp;
      draw_tree ~draw:draw ~eye:eye pos
    end

(* create_from_plane:
 * create new tree from PLANE
 * shp formed by PLANE clipped agains universe box with side-length DIMENSION
 *)
let create_from_plane ~dim ~mneg ~mpos p = 
  let shp = Polygon.create_slab_from_plane ~ccw:true ~dim:dim p in
  if shp != [] then
    (* universe box is split in half by plane *)
    Node (p, shp, Cell mneg, Cell mpos)
  else
    (* universe box is completely on one side of the plane *)
    match Plane.classify_point p V3.zero with
    | FTest.Negative -> Cell mneg
    | FTest.Positive -> Cell mpos
    | FTest.Epsilon -> failwith "BspTree3d.create_from_plane"
  
(* create_from_polyhedron:
 * create new tree from POLYHEDRON
 * -> see create_from_plane
 *)
let create_from_polyhedron ~dim ~mat ph =
  let from_plane = create_from_plane ~dim:dim ~mneg:mat ~mpos:0 in
  (* randomize order of planes to SLOW THINGS DOWN? *)
  (*let ph = Ex.List.shuffle ph in*)
  let shps = List.map from_plane ph in
    List.fold_left intersection (Cell mat) shps
  
(* to_faces:
 * make representation suitable for polygon-based rendering
 *)
let split_face_fold hp (nfaces,pfaces) (f, m1, m2) =
  match Plane.classify_points hp f with
  | FTest.NegSpace ->
      ((f, m1, m2) :: nfaces, pfaces)
  | FTest.PosSpace ->
      (nfaces, (f, m1, m2) :: pfaces)
  | FTest.SpanSpace ->
      let neg, pos = Polygon.split hp f in
        ((neg, m1, m2) :: nfaces, (pos, m1, m2) :: pfaces)
  | FTest.EpsSpace -> failwith "BspTree3d.to_faces: split_face_fold"
  
let split_faces hp faces =
  (* Ok, it is clear what is happening here *)
  List.fold_left (split_face_fold hp) ([], []) faces
  
(* WHAT THE FUCK IS HAPPENING HERE?  I guess we are splitting the
   faces of the eps_space recusively, and tag them *)
let rec classify_faces tag faces = function
  | Cell a -> List.map (tag a) faces
  | Node (hp, _, neg, pos) ->
      let fneg, fpos = split_faces hp faces in
      let fneg = classify_faces tag fneg neg in
      let fpos = classify_faces tag fpos pos in
	fneg @ fpos

let rec to_faces ?(filterb=(!=)) ?(filterf=(!=)) = function
  | Cell _ -> DCell
  | Node (hp, shp, neg, pos) ->
      (* no particular reason for 0 here, just a placeholder, really *)
    let faces = [shp, 0, 0] in
    (* classify against both subtrees *)
    let faces = classify_faces tag_neg faces neg in
    let faces = classify_faces tag_pos faces pos in
    (* find front and backfaces, reject other variants *)
    let bfaces = List.filter (fun (_, a, b) -> filterb a b) faces in
    let ffaces = List.filter (fun (_, a, b) -> filterf a b) faces in
    (* reverse winding of backfaces *)
    let rev_winding (p, m1, m2) = (List.rev p, m1, m2) in
    let bfaces = List.map rev_winding bfaces in
    (* reverse application of to_faces *)
    let neg = to_faces ~filterb:filterb ~filterf:filterf neg in
    let pos = to_faces ~filterb:filterb ~filterf:filterf pos in
    DNode (hp, bfaces, ffaces, neg, pos)
  
and tag_neg m1 (f, _, m2) = (f, m1, m2)
and tag_pos m2 (f, m1, _) = (f, m1, m2)

(* draw_none:
 *)
let draw_none _ _ = ()

(* draw_faces:
 *)
let rec draw_faces ?(drawb=draw_none) ?(drawf=draw_none) ~eye = function
| DCell -> ()
| DNode (hp, back, front, neg, pos) ->
    if Plane.distance_to_point hp eye < 0.0 then begin
      (* pos is facing away from viewer, show backfaces *)
      draw_faces ~drawb:drawb ~drawf:drawf ~eye:eye pos;
      List.iter (drawb hp) back;
      (*List.iter (drawf hp) front;*)
      draw_faces ~drawb:drawb ~drawf:drawf ~eye:eye neg
    end else begin
      (* neg "" *)
      draw_faces ~drawb:drawb ~drawf:drawf ~eye:eye neg;
      (*List.iter (drawb hp) back;*)
      List.iter (drawf hp) front;
      draw_faces ~drawb:drawb ~drawf:drawf ~eye:eye pos
    end

(* ah, the beauty of functional languages ... *)
