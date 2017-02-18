open Vec

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

let get_ray camera u v = {
    Ray.origin = camera.origin;
    Ray.dir = sub (add camera.lower_left (add (s_mult u camera.horizontal) (s_mult v camera.vertical))) camera.origin;
}
