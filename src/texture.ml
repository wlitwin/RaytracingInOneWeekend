open Util
open Objects

(* scatter : Ray.t -> hit_record -> (Vec.t attenuation * Ray.t scattered) option *)

let scatter_lambert (albedo : Vec.t) (ray : Ray.t) (hit_rec : Objects.hit_record) =
    let target = hit_rec.p +. hit_rec.normal +. random_in_unit_sphere() in
    let scattered = Ray.mk hit_rec.p (target -. hit_rec.p) in
    Some (albedo, scattered)
;;

let scatter_metal (albedo : Vec.t) (fuzz : float) (ray : Ray.t) (hit_rec : Objects.hit_record) =
    let open Ray in
    let reflected = Vec.reflect (Vec.norm ray.dir) hit_rec.normal in
    let scattered = Ray.mk hit_rec.p (reflected +. (Vec.s_mult fuzz (random_in_unit_sphere()))) in
    if Vec.dot scattered.dir hit_rec.normal > 0. then
        Some (albedo, scattered)
    else 
        None
;;

let scatter ray hit_rec =
    match hit_rec.material with
    | Lambert albedo -> scatter_lambert albedo ray hit_rec
    | Metal (albedo, fuzz) -> 
            let fuzz = if fuzz < 1. then fuzz else 1. in
            scatter_metal albedo fuzz ray hit_rec
;;
