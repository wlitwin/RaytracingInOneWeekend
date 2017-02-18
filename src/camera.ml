open Vec

let pi = 4.0 *. atan 1.0

type camera = {
    lower_left : Vec.t;
    horizontal : Vec.t;
    vertical   : Vec.t;
    origin     : Vec.t;
}

let default_camera = {
    lower_left = {x = -2.; y = -1.; z = -1.};
    horizontal = {x =  4.; y =  0.; z =  0.};
    vertical   = {x =  0.; y =  2.; z =  0.};
    origin     = Vec.zero;
}

let from_vfov_aspect vfov aspect =
    let theta = vfov *. pi /. 180. in
    let half_height = tan (theta /. 2.) in
    let half_width = aspect *. half_height in
    { lower_left = {x = -.half_width; y = -.half_height; z = -.1.0};
      horizontal = {x = 2.0*.half_width; y = 0.; z = 0.};
      vertical = {x = 0.; y = 2.0*.half_height; z = 0.};
      origin = Vec.zero;
    }
;;

let look_at from at vup vfov aspect =
    let theta = vfov *. pi /. 180. in
    let half_height = tan (theta /. 2.) in
    let half_width = aspect *. half_height in
    let w = Vec.norm (Vec.sub from at) in
    let u = Vec.norm (Vec.cross vup w) in
    let v = Vec.cross w u in
    let origin = from in
    let lower_left = Vec.sub (Vec.sub (Vec.sub origin (Vec.s_mult half_width u)) (Vec.s_mult half_height v)) w in
    let horizontal = Vec.s_mult (2. *. half_width) u
    and vertical = Vec.s_mult (2. *. half_height) v in
    { lower_left;
      horizontal;
      vertical;
      origin;
    }
;;

let get_ray camera u v = {
    Ray.origin = camera.origin;
    Ray.dir = sub (add camera.lower_left (add (s_mult u camera.horizontal) (s_mult v camera.vertical))) camera.origin;
}
