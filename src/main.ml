open Util
open Ppm
open Vec
open Ray
open Objects

let color ray objs : Vec.t =
    match hit_many ray 0.0 max_float objs with
    | Some {t;p;normal=n} -> s_mult 0.5 {x=n.x + 1.; y=n.y + 1.; z=n.z + 1.}
    | None ->
        let n_dir = norm ray.dir in
        let t : float = 0.5 * (n_dir.y + 1.0) in
        (add (s_mult (1.0 - t) (Vec.mk 1.0)) (s_mult t {x=0.5;y=0.7;z=1.0}))
;;

let _ =
    let nx = 200
    and ny = 100 in
    let img = create_image nx ny in
    let lower_left = {x= -2.;y= -1.;z= -1.}
    and horizontal = {x=4.;y=0.;z=0.}
    and vertical = {x=0.;y=2.;z=0.}
    and origin = Vec.zero in
    let s1 = Sphere ({x=0.;y=0.;z= -1.}, 0.5)
    and s2 = Sphere ({x=0.;y= -100.5;z= -1.}, 100.) in
    let objs = [s1;s2] in
    iter_image (fun x y _ ->
            let u = float x / float nx
            and v = float y / float ny in
            let u_vec = s_mult u horizontal
            and v_vec = s_mult v vertical in
            let ray = Ray.mk origin (add lower_left (add u_vec v_vec)) in
            let color = vec_to_color (color ray objs) in
            set_pixel img x y color
    ) img;
    write_ppm img "out.ppm"
;;
