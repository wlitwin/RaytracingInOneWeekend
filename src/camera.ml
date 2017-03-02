open Vec
open Rand

type camera = {
    lower_left  : Vec.t;
    horizontal  : Vec.t;
    vertical    : Vec.t;
    origin      : Vec.t;
    u           : Vec.t;
    v           : Vec.t;
    w           : Vec.t;
    lens_radius : float;
    time0       : float;
    time1       : float;
}

let rec rand_in_unit_disk () =
    let p = Vec.sub {x=2.0*.randf();y=2.0*.randf();z=0.} {x=1.;y=1.;z=0.} in
    if Vec.dot p p >= 1. then
        rand_in_unit_disk()
    else
        p
;;

(*
let default_camera = {
    lower_left  = {x = -2.; y = -1.; z = -1.};
    horizontal  = {x =  4.; y =  0.; z =  0.};
    vertical    = {x =  0.; y =  2.; z =  0.};
    origin      = Vec.zero;
    lens_radius = 0.0;
}

let from_vfov_aspect vfov aspect =
    let theta = vfov *. pi /. 180. in
    let half_height = tan (theta /. 2.) in
    let half_width = aspect *. half_height in
    { lower_left = {x = -.half_width; y = -.half_height; z = -.1.0};
      horizontal = {x = 2.0*.half_width; y = 0.; z = 0.};
      vertical = {x = 0.; y = 2.0*.half_height; z = 0.};
      origin = Vec.zero;
      lens_radius = 0.0;
    }
;;
*)

let look_at from at vup vfov aspect aperature focus_dist time0 time1 =
    let theta = vfov *. pi /. 180. in
    let half_height = tan (theta /. 2.) in
    let half_width = aspect *. half_height in
    let w = Vec.norm (Vec.sub from at) in
    let u = Vec.norm (Vec.cross vup w) in
    let v = Vec.cross w u in
    let origin = from in
    let lower_left = Vec.sub (Vec.sub (Vec.sub origin (Vec.s_mult (half_width*.focus_dist) u)) (Vec.s_mult (half_height*.focus_dist) v)) (Vec.s_mult focus_dist w) in
    let horizontal = Vec.s_mult (2. *. half_width *. focus_dist) u
    and vertical = Vec.s_mult (2. *. half_height *. focus_dist) v in
    { lower_left;
      horizontal;
      vertical;
      origin;
      u;
      v;
      w;
      lens_radius = aperature /. 2.0;
      time0;
      time1;
    }
;;

let get_ray (camera, u, v) = 
    let rd = s_mult camera.lens_radius (rand_in_unit_disk()) in
    let offset = add (s_mult rd.x camera.u) (s_mult rd.y camera.v) in
    { Ray.origin = add camera.origin offset;
      Ray.dir = sub (sub (add camera.lower_left (add (s_mult u camera.horizontal) (s_mult v camera.vertical))) camera.origin) offset;
      Ray.time = camera.time0 +. (randf() *. (camera.time1 -. camera.time0));
      Ray.wavelength = 0.;
    }
;;
