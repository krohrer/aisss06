(*
    Main Module
*)

open Math3d
open Geom3d

class glut_main =
object (self)

  val vdir_init = V3.ex
  val mutable vpos = (-5.0, 0.0, 1.5)
  val mutable vdir = V3.ex
  val mutable vrot = V3.zero
  val mutable vvel = 0.0
  val mutable drot = V3.zero
  val mutable walk = false
  val rot_angle = acos 0.0 *. 0.1
  
  val mutable seed = Random.bits ();
  val mutable lod = -1
  val mutable bsptree = BspTree3d.empty
  val mutable ftree = BspTree3d.to_faces BspTree3d.empty
  val mutable iworld = 0

  val mutable draw_tree = false
  val mutable draw_faces = true
  
  val mutable alpha = 0.5
  val mutable light = false
  val mutable blend = true
  val mutable outline = true
  val mutable fullscreen = false
  val mutable width = 512
  val mutable height = 512
  val mutable aspect = 1.0
  
  method reshape ~w ~h =
    if not fullscreen then begin
      width <- w;
      height <- h
    end;
    aspect <- float_of_int w /. float_of_int h;
    GlDraw.viewport ~x:0 ~y:0 ~w:w ~h:h

  method world = Proc.worlds.(iworld mod Array.length Proc.worlds)

  method init =
    let dim = 1000.0 in
    let tree = Proc3d.emerge
      ~seed:[| seed |]
      ~dim:dim
      ~lod:lod
      ~transform:AT3.identity
      self#world
    in
      bsptree <- tree;
      ftree <- BspTree3d.to_faces
	~filterb:Proc.filter_front
	~filterf:Proc.filter_back
	tree

  method render () =
    GlMat.mode `projection;
    GlMat.load_identity ();
    GluMat.perspective
      ~fovy:90.0
      ~aspect:aspect
      ~z:(0.1, 2000.);
    GluMat.look_at
      ~eye:vpos
      ~center:(V3.add vpos vdir)
      ~up:(0., 0., 1.);

    GlMat.mode `modelview;
    GlMat.load_identity ();

    GlClear.color ~alpha:0.0 (0.8, 0.8, 1.0);
    GlClear.clear [`color; `depth];
    Gl.disable `depth_test;

    GlDraw.shade_model `smooth;
    Gl.enable `normalize;
    Gl.enable `lighting;
    Gl.enable `color_material;
    GlLight.color_material ~face:`both `ambient_and_diffuse;
    GlLight.material ~face:`both (`specular (1.0, 1.0, 1.0, 1.0));
    GlLight.light_model (`ambient (0.6, 0.6, 0.6, 1.0));

    GlMat.push ();
    Gl.enable `light0;
    GlLight.light 0 (`diffuse  (0.8, 0.4, 0.4, 1.0));
    GlLight.light 0 (`specular (0.0, 0.0, 0.0, 0.0));
    GlLight.light 0 (`position (-0.4, 0.2, 1.0, 0.0));
    GlMat.translate3 vpos;

    if light then Gl.enable `light1 else Gl.disable `light1;
    GlLight.light 1 (`diffuse  (0.5, 0.5, 0.5, 1.0));
    GlLight.light 1 (`specular (0.0, 0.0, 0.0, 0.0));
    GlLight.light 1 (`position (0.0, 0.0, 0.0, 1.0));
    GlLight.light 1 (`quadratic_attenuation 0.001);
    GlMat.pop ();

    let draw_poly1 hp p =
      let n = Plane.normal hp in
	GlDraw.normal3 n;
	GlDraw.color ~alpha:0.1 (V3.mul1 (V3.add V3.one (Plane.normal hp)) 0.5);
	GlDraw.color ~alpha:0.3 V3.zero;
	GlDraw.begins `line_loop;
        List.iter GlDraw.vertex3 p;
	GlDraw.ends ()
    in
    
    Gl.enable `lighting;
    if blend or (alpha < 1.) then Gl.enable `blend else Gl.disable `blend;
    GlFunc.blend_func `src_alpha `one_minus_src_alpha;
    Gl.enable `cull_face;
    GlDraw.cull_face `back;
    Gl.enable `depth_test;
      
    GlFunc.depth_func `always;
    
    GlDraw.line_width 1.0;
    Gl.enable `line_smooth;
    
    if draw_faces then
      BspTree3d.draw_faces
        ~drawb:(Proc.draw_face ~alpha outline true)
        ~drawf:(Proc.draw_face ~alpha outline false)
        ~eye:vpos ftree;
      
    Gl.disable `lighting;
    GlFunc.depth_func `less;
        
    if draw_tree then
      BspTree3d.draw_tree
        ~draw:(draw_poly1)
        ~eye:vpos bsptree;      
    
    if draw_tree then begin
      GlMat.push ();
      GlMat.scale3 (V3.mul1 V3.one 5.);
      Gl.disable `lighting;
        GlDraw.begins `lines;
          GlDraw.color V3.ex;
          List.iter GlDraw.vertex3 [V3.zero;V3.ex];
          GlDraw.color V3.ey;
          List.iter GlDraw.vertex3 [V3.zero;V3.ey];
          GlDraw.color V3.ez;
          List.iter GlDraw.vertex3 [V3.zero;V3.ez];
        GlDraw.ends ();
      GlMat.pop ()
    end;

    Gl.flush ();
    Glut.swapBuffers ()

  method keyboard_down ~key ~x ~y =
    Glut.postRedisplay ();
    match char_of_int key with
    | '\027' -> exit 0
    | 'i' | 'I' ->
        seed <- Random.bits ();
        self#init
    | 'p' | 'P' ->
        let follow ~src ~field ~dst =
	  Obj.tag dst < Obj.no_scan_tag && Obj.size dst = 4
	in
	let context = Inspect.Dot.make_context ~max_fields:0 ~follow () in
	  Inspect.Dot.dump_osx ~context ~cmd:"dot" bsptree
    | 'o' | 'O' ->
	let context = Inspect.Dot.make_context ~max_fields:0 () in
	  Inspect.Dot.dump_osx ~context self#world
    | 'w' | 'W' ->
        vvel <- vvel +. 0.5;
        self#update_pos
    | 's' | 'S' ->
        vvel <- vvel -. 0.5;
        self#update_pos
    | 'a' | 'A' ->
        vpos <- V3.add vpos (V3.cross (0.0,0.0,0.4) vdir);
        self#update_pos
    | 'd' | 'D' ->
        vpos <- V3.sub vpos (V3.cross (0.0,0.0,0.4) vdir);
        self#update_pos
    | 'q' | 'Q' ->
        vpos <- V3.add vpos (V3.sz 0.5)
    | 'e' | 'E' ->
        vpos <- V3.sub vpos (V3.sz 0.5)
    | 'f' | 'F' ->
        drot <- V3.add drot (V3.sz rot_angle);
        self#update_pos;
    | 'h' | 'H' ->
        drot <- V3.sub drot (V3.sz rot_angle);
        self#update_pos
    | 't' | 'T' ->
        drot <- V3.add drot (V3.sy rot_angle);
        self#update_pos;
    | 'g' | 'G' ->
        drot <- V3.sub drot (V3.sy rot_angle);
        self#update_pos
    | 'b' | 'B' ->
        blend <- not blend;
    | '+' | '=' ->
	alpha <- min (alpha +. 0.1) 1.
    | '-' | '_' ->
	alpha <- max (alpha -. 0.1) 0.
    | 'l' | 'L' ->
        light <- not light;
    | 'n' | 'N' ->
        outline <- not outline;
    | ',' ->
        draw_faces <- not draw_faces
    | '.' ->
        draw_tree <- not draw_tree
    | '0' .. '9' ->
        lod <- ~-(key - (int_of_char '0'));
        self#init
    | ' ' ->
        walk <- not walk;
        if walk then begin
          Glut.idleFunc ~cb:(Some self#tick)
        end else begin
          Glut.idleFunc ~cb:(None);
          vvel <- 0.0;
          drot <- V3.zero;
        end
    | '[' ->
        if iworld > 0 then
          iworld <- pred iworld
        else
          iworld <- Array.length Proc.worlds;
        self#init
    | ']' ->
        iworld <- succ iworld;
        self#init
    | '\t' ->
        if fullscreen then begin
          fullscreen <- false;
          Glut.reshapeWindow ~w:width ~h:height
        end else begin
          fullscreen <- true;
          Glut.fullScreen ()
        end
    | _ -> ()
    
(*
  method keyboard_up ~key ~x ~y =
    match char_of_int key with
    | 'w' | 'W' ->
      vvel <- 0.0;
      self#update_pos
    | 's' | 'S' ->
      vvel <- 0.0;
      self#update_pos
    | _ -> ()
*)
 
  method tick () =
    self#update_pos;
    Glut.postRedisplay ()
 
   method update_pos =
    let vv, dr =
      if walk then
        vvel *. 0.2, V3.mul1 drot 0.05
      else
        vvel, drot
    in
    vrot <- V3.add vrot dr;
    let myaw = M3.rotate_z ~angle:(V3.z vrot) in
    let mpitch = M3.rotate_y ~angle:(V3.y vrot) in
    let m = M3.mul myaw mpitch in
      vdir <- M3.mul3 m vdir_init;
      vpos <- V3.add vpos (V3.mul1 vdir vv);
      if not walk then begin
        vvel <- 0.0;
        drot <- V3.zero
      end else 
        drot <- V3.mul1 drot 0.95
   
  method run =
    let argv' = Glut.init Sys.argv in ignore argv';
    Glut.initDisplayMode ~depth:true ~double_buffer:true ();
    Glut.initWindowSize ~w:512 ~h:512;
    ignore(Glut.createWindow ~title:"AISSS06");
   
    Glut.displayFunc  ~cb:(self#render);
    Glut.idleFunc     ~cb:(None);
    Glut.keyboardFunc   ~cb:(self#keyboard_down);
    (*Glut.keyboardUpFunc ~cb:(self#keyboard_up);*)
    Glut.reshapeFunc  ~cb:(self#reshape);
    
    Random.self_init ();
    
    Glut.mainLoop ()
end

let glmain = new glut_main;;
let main = glmain#run; print_string "Hello world\n";;
print_string "Hello world";;
