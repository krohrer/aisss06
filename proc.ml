open Math2d
open Math3d
open Geom3d
open Proc3d

(*- materials ----------------------------------------------------------------*)
let water     = 1
let glass     = 2*water
let glass2    = 2*glass
let grass     = 2*glass2
let earth     = 2*grass
let wood      = 2*earth
let brick2    = 2*wood
let tarmac    = 2*brick2
let brick     = 2*tarmac
let concrete  = 2*brick
let mat_all   = 2*concrete - 1

let mat_translucent_lul = [water; grass; glass; glass2]
let mat_color_lul = [
  max_int,  (0.7, (1.0, 0.0, 1.0));
  concrete, (1.0, (0.5, 0.5, 0.5));
  brick,    (1.0, (0.8, 0.4, 0.3));
  tarmac,   (1.0, (0.1, 0.1, 0.1));
  brick2,   (1.0, (0.4, 0.0, 0.0));
  wood,     (1.0, (0.7, 0.5, 0.4));
  earth,    (1.0, (0.4, 0.3, 0.3));
  grass,    (0.8, (0.0, 0.5, 0.0));
  glass2,   (0.5, (0.0, 0.0, 0.4));
  glass,    (0.2, (0.8, 0.8, 1.0));
  water,    (0.6, (0.0, 0.2, 0.5));
]

let bsr i =
  let rec bsr' n = function 0 -> n | i -> bsr' (n+1) (i lsr 1)
    in bsr' 0 i

let mat_pure m = 1 lsl (bsr m - 1)

let mat_translucent m = List.mem (mat_pure m) mat_translucent_lul
let mat_color m = snd (List.find (fun (mc, _) -> m >= mc) mat_color_lul)

let filter_front m1 m2 =
  let m1, m2 = mat_pure m1, mat_pure m2 in
    (m1 < m2) || (List.mem m1 mat_translucent_lul && m2 != m1)
let filter_back  m1 m2 = filter_front m2 m1

let draw_face ?(alpha=1.0) outline back hp (p,m1,m2) =
  let n = if back then V3.neg (Plane.normal hp) else Plane.normal hp in
  GlDraw.normal3 n;
  let a, rgb = mat_color (max m1 m2) in
  let alpha = a *. alpha in
  GlDraw.color ~alpha rgb;
  GlDraw.begins `polygon;
    List.iter GlDraw.vertex3 p;
  GlDraw.ends ();
  if outline then begin
    GlDraw.begins `line_loop;
      List.iter GlDraw.vertex3 p;
    GlDraw.ends ()
  end

(*- Testing ------------------------------------------------------------------*)

let add_stripe w i =
  mat (1 lsl i) >>> tly 2.0 >>> unite w ubox

let stripes n =
  let nf = float n in
    scx (nf +. 2.0) >>> tly (~-.nf -. 2.0) >>>
    mfold add_stripe (unity 0) (Ex.List.from_to 0 n)

let stripe_op op =
  call (stripes 9) >>= op (call (roz 90.0 >>> stripes 9))
  
let stripe_test =
  void >>=
  pass (tly (-50.0)) >>++ (stripe_op intersect') >>=
  pass (tly ( 25.0)) >>++ (stripe_op unite'    ) >>=
  pass (tly ( 25.0)) >>++ (stripe_op diff'     ) >>=
  pass (tly ( 25.0)) >>++ (stripe_op symdiff'  )
  
(*- Houses -------------------------------------------------------------------*)

(* oriented box *)
let obox o vx vy vz =
  half o (V3.neg vx) >>**
  half o (V3.neg vy) >>**
  half o (V3.neg vz) >>**
  half (V3.add o vx) vx >>**
  half (V3.add o vy) vy >>**
  half (V3.add o vz) vz
  
(* find axes *)
let paxes ex =
  V3.of2 ex, V3.of2 (V2.ortho ex), V3.ez

(* find point *)
let ppoint' o vx vy vz (ox, oy, oz) =
  let o = V3.add o (V3.mul1 vx ox) in
  let o = V3.add o (V3.mul1 vy oy) in
  let o = V3.add o (V3.mul1 vz oz) in
    o

let ppoint o ex offs =
  let vx, vy, vz = paxes ex in
    ppoint' o vx vy vz offs

(* oriented z-aligned box *)
let zbox o ex offs (dx,dy,dz) =
  let vx, vy, vz = paxes ex in
  let o = ppoint' o vx vy vz offs in
    obox o (V3.mul1 vx dx) (V3.mul1 vy dy) (V3.mul1 vz dz)

(* helper functions to deal with zboxes *)
let ppartx x offs dims =
  let xn, xp = x, V3.x dims -. x in
  let dneg = V3.sub dims (V3.sx xp) in
  let dpos = V3.sub dims (V3.sx xn) in
  let oneg = offs in
  let opos = V3.add offs (V3.sx x) in
    (oneg, dneg), (opos, dpos)
    
let pparty y offs dims =
  let yn, yp = y, V3.y dims -. y in
  let dneg = V3.sub dims (V3.sy yp) in
  let dpos = V3.sub dims (V3.sy yn) in
  let oneg = offs in
  let opos = V3.add offs (V3.sy y) in
    (oneg, dneg), (opos, dpos)
    
let pshrink s3 offs dims =
  (V3.add offs s3), (V3.sub dims (V3.add s3 s3))
  
let pgrow s3 offs dims = pshrink (V3.neg s3) offs dims

(*- Brick houses -------------------------------------------------------------*)

(* add a window *)
let add_window o ex offs dims wld =
  let w, d, h = dims in
  let zbox os ds = zbox o ex (V3.add os offs) ds in
  let cutout =
    mat mat_all >>> zbox (-.w*.0.5, 0.0, 0.0) (w, d, h)
  in
  let ledge =
    let w, d, h = w+.0.2, d+.0.1, 0.1 in
      mat concrete >>> zbox (-.w/.2., -0.05, -.h) (w, d, h)
  in
  let wglass =
    mat glass >>> zbox (-.w*.0.5, d*.0.5, 0.0) (w, 0.02, h)
  in
  let cross =
    let hbar = zbox (-.w*.0.5, d*.0.5, h*.0.5) (w, 0.02, 0.05) in
    let vbar = zbox (-0.05, d*.0.5, 0.0) (0.1, 0.02, h) in
      mat wood >>> hbar >>++ vbar
  in
    wld -->
    ldiff 1 cutout >>=
    lunite 3 ledge >>=
    lunite 2 cross >>=
    lunite 4 wglass
    
(* add a door *)
let add_door win o ex offs dims wld =
  let w, d, h = dims in
  let zbox os ds = zbox o ex (V3.add os offs) ds in
  let cutout =
    mat mat_all >>> zbox (-.w*.0.5, 0.0, 0.0) (w, d, h)
  in
  let step =
    let w, d, h = w, d, 0.02 in
      mat concrete >>> zbox (-.w/.2., 0.0, 0.0) (w, d, h)
  in
  let door m =
    mat m >>> zbox (-.w/.2., d*.0.5, 0.0) (w, 0.05, h)
  in
  let wcutout =
    mat wood >>> zbox (-.w*.0.3, 0.0, 0.6*.h) (0.6*.w, d, 0.3*.h)
  in
  let knob =
    mat concrete >>> zbox (-.w/.2.+.0.1, d*.0.5-.0.05, 0.9) (0.1, 0.05, 0.05)
  in
    wld -->
    ldiff 1 cutout >>=
    lunite 2 (door (wood lor glass2)) >>=
    mwhen' win (ldiff 3 wcutout) >>=
    lunite 3 step >>=
    lunite 4 knob
    
(* add stairs *)
let add_stairs o ex offs dims wld =
  let w, d, h = dims in
  let zbox os ds = zbox o ex (V3.add os offs) ds in
  (* each step is only about 20 cm *)
  let steps = floor (h /. 0.2) in
  let sw, sd, sh = w, d /. (steps +. 1.0), h /. steps in
  let add_step wld i = 
    let fi = float i in
      unite wld (mat wood >>> zbox (0.0, fi*.sd, fi*.sh) (sw, 2.0*.sd, sh))
  in
  let slst = Ex.List.from_to 0 ((int_of_float steps) - 1) in
    wld -->
    lod 1 (mfold' add_step slst)
    
(* add a roof *)
let add_roof m o ex offs dims wld =
  let o1, d1 = pgrow (0.1, 0.1, 0.0) offs dims in
  let o2, d2 = pshrink (0.2, 0.2, 0.0) offs dims in
  let o2 = V3.add o2 (V3.sz 0.1) in
    wld -->
    lunite 1 (mat m >>> zbox o ex o1 d1) >>=
    ldiff 2 (mat mat_all >>> zbox o ex o2 d2)

(* add fundament *)
let add_fundament o ex offs dims wld =
  let offs, dims = pgrow (0.1, 0.1, 0.0) offs dims in
    wld --> lunite 0 (mat concrete >>> zbox o ex offs dims)

(* add staircase *)
let add_staircase i o ex offs dims wld =
  let zbox os ds = zbox o ex os ds in
  let ro, rd = pshrink (0.15, 0.3, 0.0) offs dims in
  let cutaway =
      mat mat_all >>> zbox ro rd
  in
  let add_windows wld =
    let w, d, _ = dims in
    let ox, oy, oz = offs in
    let ox = ox +. 0.5 *. w in
      wld -->
      mwhen' (i=0) (add_door true o ex (ox, oy, oz) (1.0,0.3,2.0)) >>=
      munless' (i=0) (add_window o ex (ox, oy, oz +. 0.8) (1.0,0.3,1.2)) >>=
      add_window o ex (ox, oy +. d -. 0.3, oz +. 0.8) (1.0,0.3,1.2)
  in
  let add_stairs wld =
    let _, (so, sd) = ppartx 1.4 ro rd in
    let so, sd = pshrink (0.0, 1.0, 0.0) so sd in
    let sd = V3.add sd (V3.sz 0.2) in
      wld -->
      ldiff 2 (mat mat_all >>> zbox so sd) >>=
      lod 1 (add_stairs o ex so sd)
  in
  let add_door wld =
    let o = ppoint o ex (V3.add ro (0.0, 1.0, 0.0)) in
      wld --> add_door false o (V2.ortho ex) V3.zero (1.0,0.3,2.0)
  in
    wld -->
    ldiff 1 cutaway >>=
    add_stairs >>=
    lod 1 add_door >>=
    add_windows

(* add flat *)
let add_flat o ex offs dims wld =
  let w, d, h = dims in
  let zbox os ds = zbox o ex (V3.add os offs) ds in
  let wins = floor (w /. 2.0) in
  let wspacing = w /. wins in
  let add_windows wld fi =
    let ox, oy, oz = offs in
    let ox = ox +. (fi +. 0.5) *. wspacing in
    let oz = oz +. 0.8 in
      wld -->
      add_window o ex (ox, oy, oz) (1.0,0.3,1.2) >>=
      add_window o ex (ox, oy +. d -. 0.3, oz) (1.0,0.3,1.2)
  in
  let cutaway =
    let ro, rd = pshrink (0.15, 0.3, 0.0) V3.zero dims in
      mat mat_all >>> zbox ro rd
  in
  let add_stuffing wld =
    perform
      ro <-- rnd3 (0.15, 0.3, 0.0) (w -. 1.15, d -. 1.3, 0.0);
      rd <-- rnd3 (0.5, 0.5, 0.5) V3.one;
      rm <-- rndi 0 10;
      wld --> lunite 5 (mat (1 lsl rm) >>> zbox ro rd)
  in
  let wlst = Ex.List.from_to 0 (int_of_float wins - 1) in
  let wlst = List.map float wlst in
    wld -->
    ldiff 1 cutaway >>=
    mrep 4 add_stuffing >>=
    mfold' add_windows wlst

(* add one floor *)
let add_floor (mw, mf) o ex offs dims wld i =
  let zbox os ds = zbox o ex (V3.add os offs) ds in
  let bricks =
    mat mw >>> zbox V3.zero dims
  in
  let ceiling =
    let cw, cd, ch = dims in
    let co, cd = pgrow (0.1, 0.1, 0.0) (V3.sz ch) (cw, cd, -0.2) in
      mat mf >>> zbox co cd
  in
  let (fo, fd), (so, sd) =
    ppartx (V3.x dims -. 3.0) offs (V3.sub dims (V3.sz 0.2))
  in
    wld -->
    lunite 0 bricks >>=
    lunite 1 ceiling >>=
    lod 1 (add_flat o ex fo fd) >>=
    lod 1 (add_staircase i o ex so sd)
    
(* add a house *)	
let add_brickhouse (mw, mf, mr) floors o ex dims wld =
  let w, d, h = dims in
  let add_floor wld i =
    add_floor (mw, mf) o ex (V3.sz (0.2 +. h *. float i)) dims wld i
  in
  let flst = Ex.List.from_to 0 (floors-1) in
    wld -->
    add_fundament o ex V3.zero (w, d, 0.2) >>=
    mfold' add_floor flst >>=
    add_roof mr o ex (V3.sz (0.2 +. h *. float floors)) (w, d, 0.4)
  
(*- Glass houses -------------------------------------------------------------*)

let add_glasshouse (gm, fm) floors o ex dims wld =
  let w, d, h = dims in
  let zbox os ds = zbox o ex os ds in
  let fundament =
    mat concrete >>> zbox V3.zero (w, d, 0.2)
  in
  let cutaway os ds =
    let os, ds = pshrink (0.05, 0.05, 0.0) os ds in
      mat mat_all >>> zbox os ds
  in
  let add_stuffing f wld =
    perform
      ro <-- rnd3 (0.5, 0.5, f *. h -.h) (w -. 4.5, d -. 4.5, f *. h -. h);
      rd <-- rnd3 V3.one (4.0,4.0,2.0);
      rm <-- rndi 0 10;
      wld --> lunite 5 (mat (1 lsl rm) >>> zbox ro rd)
  in
  let add_floor wld i =
    wld -->
    lunite 1 (mat fm >>> zbox (0.0, 0.0, h *. float i) (w, d, 0.2)) >>=
    mrep 3 (add_stuffing (float i))
  in
  let add_elevator wld =
    let os = (1.0, d -. 4.0, 0.0) in
    let ds = (3.0,3.0,h*.float floors) in
      wld --> lunite 2 (mat fm >>> zbox os ds)
  in
  let hd = V3.mul (1.0,1.0,float floors) dims in
  let ho = V3.sz 0.2 in
  let clst = Ex.List.from_to 0 floors in
    wld -->
    lunite 1 fundament >>=
    lunite 0 (mat gm >>> zbox ho hd) >>=
    ldiff 1 (cutaway ho hd) >>=
    mfold' add_floor clst >>=
    add_elevator
    
(*- Vegetation ---------------------------------------------------------------*)

let tree (h, r, c) =
  let trunk = mat wood >>> sc3 (r,r,h) >>> tlz 1.0 >>> ucylinder in
  let crown = mat grass >>> tlz (2.0 *. h +. c) >>> sc1 c >>> detail 1 usphere ubox in
    call crown >>= lunite 1 trunk

let add_trees pmin pmax n w =
  let rtree =
    call (rnd3 pmin pmax >>= tl3 >>> rnd3 (2.0,0.3,1.5) (3.0,0.5,2.5) >>= tree)
  in
    mrep n (unite' rtree) w

let add_ground z w =
  mat earth >>> unite w (half (V3.sz z) V3.ez)
  
(*- Yadayada -----------------------------------------------------------------*)

let add_rhouse o ex wld =
  perform
    r <-- rndi 2 10;
    r2 <-- rnd 0.0 1.0;
    rm <-- rndi 0 3;
    r3 <-- rnd3 (6.0,5.0,2.5) (10.0,8.0,3.2);
    let m = match rm with 0 -> brick | 1 -> brick2 | _ -> concrete in
    wld <-- call (tly r2 >>> add_brickhouse (m,concrete,concrete) r o V2.ex r3 wld);
    tlx (V3.x r3);
    ret wld

let world1 =
perform
  r <-- rndi 5 15;
  void >>=
  lunite 2 (mat grass >>> zbox (-20.0, -28.0, 0.0) V2.ex V3.zero (40.0, 20.0, 0.01))>>=
  lunite 1 (mat tarmac >>> zbox (-100.0, -6.0, 0.0) V2.ex V3.zero (200.0, 4.0, 0.01)) >>=
  lunite 2 (mat concrete >>> zbox (-100.0, -2.0, 0.0) V2.ex V3.zero (200.0, 2.0, 0.2)) >>=
  lunite 2 (mat concrete >>> zbox (-100.0, -8.0, 0.0) V2.ex V3.zero (200.0, 2.0, 0.2)) >>=
  add_ground 0.0 >>=
  add_glasshouse (glass2, tarmac) r (0.0, -28.0, 0.0) V2.ex (20.0, 20.0, 3.0) >>=
  add_trees (-18.0, -20.0, 0.0) (-2.0, -10.0, 0.0) 5 >>=
  pass (tlx ~-.20.0) >>=
  mrep 4 (add_rhouse V3.zero V2.ex)
  
(* Yada two ------------------------------------------------------------------*)

let world2 =
  let add_roundhouse wld =
    tlx 10.0 >>> roz 120.0 >>> add_rhouse V3.zero V2.ex wld
  in
  perform
    void >>=
    add_ground 0.0 >>=
    mrep 3 add_roundhouse >>=
    lunite 0 (tlz 1.2 >>> mat water >>> half V3.zero V3.ez)
    
(* Yada three ----------------------------------------------------------------*)

let world3 =
  let add_block wld =
    tlz 2.0 >>> roz 15.0 >>>
    unite wld (zbox V3.zero V2.ex (-2.0,-0.5,-2.0) (4.0,1.0,2.0))
  in
  let add_spiral wld i =
    mat (1 lsl i) >>> tlx 5.0 >>>
    call (mrep 12 add_block wld)
  in
  let add_spirals wld =
    let slist = Ex.List.from_to 0 (bsr concrete - 1) in
      mfold' add_spiral slist wld
  in
  perform
    void >>=
    add_ground 0.0 >>=
    add_spirals

(* Yada Four ----------------------------------------------------------------*)
    
let ppartx x ((ox,oy,oz) as o, d) = ppartx x o d
let pparty y ((ox,oy,oz) as o, d) = pparty y o d
let pshrink s3 (o, d) = pshrink s3 o d
let pgrow s3 (o, d) = pgrow s3 o d
let parea (_, (x, y, _)) = x *. y
let zbox (o, d) = zbox V3.zero V2.ex o d

let pplace dir (o, d) =
  let (ox,oy,oz), (dx,dy,dz) = o, d in
  match dir with
    0 -> (o,d), V2.ex
  | 1 -> ((ox+.dx,oy,oz), V3.yxz d), V2.ey
  | 2 -> ((ox+.dx,oy+.dy,oz), d), V2.neg V2.ex
  | 3 -> ((ox,oy+.dy,oz), V3.yxz d), V2.neg V2.ey
  | _ -> failwith "Proc.pplace"

let add_trees (ot, dt) nt = add_trees ot (V3.add ot dt) nt

let add_brickhouse ex (oh, dh) wld =
  perform
    floors <-- rndi 1 4;
    fh <-- rnd 2.4 3.5;
    mi <-- rndi 0 4;
    let ms = [|
      brick, concrete, concrete;
      brick2, tarmac, concrete;
      brick, brick2, concrete;
      tarmac, concrete, concrete
    |] in
    let dh = V3.x dh, V3.y dh, fh in
      wld -->
      add_brickhouse ms.(mi) floors oh ex dh

let add_glasshouse (oh, dh) wld =
  perform
    floors <-- rndi 5 15;
    fh <-- rnd 3.5 4.5;
    mi <-- rndi 0 3;
    let dh = V3.x dh, V3.y dh, fh in
    let ms = [|
      glass, concrete;
      glass2, concrete;
      glass2, tarmac
    |] in
      wld -->
      add_glasshouse ms.(mi) floors oh V2.ex dh

let qdiv (x,y) grow shrink odim =
  let odim = pgrow (V3.of2 grow) odim in
  let odn, odp = ppartx x odim in
  let odnn, odnp = pparty y odn in
  let odpn, odpp = pparty y odp in
  let shr = pshrink (V3.of2 shrink) in
    shr odnn, shr odnp, shr odpn, shr odpp
    
let add_park odim wld =
  let odt = pshrink (1.0, 1.0, 0.0) odim in
  perform
    nt <-- rndi 2 3;
    wld -->
    lunite 1 (tlz 0.1 >>> mat grass >>> zbox odim) >>=
    add_trees odt nt
    
let add_block house odim wld =
  let odh = pshrink (1.5, 1.5, 0.0) odim in
  let w, d, _ = snd odh in
  let maxd = max (abs_float w) (abs_float d) in
    wld -->
    lunite 2 (mat concrete >>> zbox odim) >>=
    if maxd < 4.0 then
      add_park odim
    else
      house odh

let add_brickhouses odim wld =
  let (_, (w,d,h)) = odim in
  let add_house dir odim wd =
    let (o, _), ex = pplace dir odim in
      add_brickhouse ex (o,wd)
  in
  let add_ns odim wd wld =
    wld -->
    add_house 0 odim wd >>=
    add_house 2 odim wd
  in
  let add_ew odim wd wld =
    wld -->
    add_house 1 odim (V3.yxz wd) >>=
    add_house 3 odim (V3.yxz wd)
  in
  let add_4 odim wd wld =
    wld -->
    add_ns odim wd >>=
    add_ew odim wd
  in
    wld -->
    if w > 12.0 then (
      if d > 12.0 then
        add_4 odim (w/.2.2, d/.2.2, h)
      else
        add_ew odim (w/.2.2, d, h)
    ) else (
      if d > 12.0 then
        add_ns odim (w, d/.2.2, h)
      else (
        if w > d then
          add_house 0 odim (w, d, h)
        else
          add_house 1 odim (d, w, h)
      )
    )

let rec qcity i d odim wld =
  let w, h, _ = snd odim in
  perform
  pp <-- rndi 0 5;
  if pp < i then
    wld --> add_block add_park odim
  else if min w h > 30.0 then begin
    perform
      let dx, dy, _ = snd odim in
      sx <-- rnd 0.4 0.6;
      sy <-- rnd 0.4 0.6;
      let odnn, odnp, odpn, odpp =
        qdiv (sx*.dx, sy*.dy) d d odim
      in
      let i = succ i in
      let d = V2.mul1 d 0.8 in
      wld -->
      qcity i d odnn >>=
      qcity i d odnp >>=
      qcity i d odpn >>=
      qcity i d odpp
  end else begin
    perform
      d <-- rnd 0.0 1.0;
      wld -->
      if d > 0.3 then
        add_block add_brickhouses odim
      else if d > 0.2 then
        add_block add_glasshouse odim
      else
        add_block add_park odim
(*
      else
        ldiff 1 (tlz (-0.2) >>> mat mat_all >>> zbox odim)
*)
  end

let world4 =
  let o, d = (-50.0, -50.0, -0.2), (100.0, 100.0, 0.2) in
    void >>=
    add_ground (-0.4) >>=
    lunite 1 (mat tarmac >>> zbox (V3.sub o (V3.sz 0.2), d)) >>=
    qcity 0 (4.0, 4.0) (o, d)

(*- Yada five ----------------------------------------------------------------*)

(* same as world4, but bigger dimensions *)
let world5 =
  let o, d = (-200.0, -200.0, -0.2), (400.0, 400.0, 0.2) in
    void >>=
    add_ground (-0.4) >>=
    lunite 1 (mat tarmac >>> zbox (V3.sub o (V3.sz 0.2), d)) >>=
    qcity (-4) (4.0, 4.0) (o, d)

(*- Yada six -----------------------------------------------------------------*)

let world6 =
  perform
    wld <--
      void >>=
      add_ground 0.0 >>=
      call1 (add_rhouse V3.zero V2.ex);
    sub <-- mat mat_all >>> half V3.zero (-1.0,-2.0,1.0);
    intersect sub (ret wld)

(*- export worlds ------------------------------------------------------------*)

let worlds = [|world1; world2; world6; world4; world5; world3|]
  
(*- Fin ----------------------------------------------------------------------*)
