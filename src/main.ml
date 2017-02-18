open Util
open Ppm
open Vec
open Ray
open Objects
open Texture

let _ = Random.self_init ()

let rec shoot_ray ray objs depth : Vec.t =
    match hit_many ray 0.001 max_float objs with
    | Some hit_rec -> 
            if depth < 50 then
                match scatter ray hit_rec with
                | Some (attenuation, scattered) ->
                    attenuation *. (shoot_ray scattered objs (iadd depth 1))
                | None -> Vec.zero
            else
                Vec.zero
    | None ->
        let n_dir = norm ray.dir in
        let t : float = 0.5 * (n_dir.y + 1.0) in
        (add (s_mult (1.0 - t) Vec.one) (s_mult t {x=0.5;y=0.7;z=1.0}))
;;

let sample_ray camera objs (samps : int) (w : float) (h : float) x y =
    let rec loop i color = 
        match i with
        | 0 -> color
        | n ->
            let u = (x + randf()) / w
            and v = (y + randf()) / h in
            let ray = Camera.get_ray camera u v in
            loop (isub n 1) (color +. shoot_ray ray objs 0)
    in
    let color = loop samps Vec.zero in
    s_div (float samps) color
;;

let _ =
    let nx = 1000
    and ny = 500
    and ns = 1000 in
    let img = create_image nx ny in
    let s1 = Sphere ({x=0.;y=0.;z= -1.}, 0.5, Lambert (Vec.mk 0.8 0.3 0.3))
    and s2 = Sphere ({x=0.;y= -100.5;z= -1.}, 100., Lambert (Vec.mk 0.8 0.8 0.0))
    and s3 = Sphere ({x=1.;y=0.;z= -1.}, 0.5, Metal (Vec.mk 0.8 0.6 0.2), 0.3)
    and s4 = Sphere ({x= -1.;y= 0.;z= -1.}, 0.5, Metal (Vec.mk 0.8 0.8 0.8, 1.0)) in
    let objs = [s1;s2;s3;s4] in
    let camera = Camera.default_camera in
    let sample_ray = sample_ray camera objs ns (float nx) (float ny) in
    iter_image (fun x y _ ->
        sample_ray (float x) (float y)
        |> op sqrt
        |> vec_to_color 
        |> set_pixel img x y
    ) img;
    write_ppm img "out.ppm"
;;
