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

let scatter_dielectric (ref_idx : float) (ray : Ray.t) (hit_rec : Objects.hit_record) =
    let open Ray in
    let attenuation = Vec.mk 1. 1. 1. in
    let out_norm, ni_over_nt, cosine =
        if Vec.dot ray.dir hit_rec.normal > 0. then
            Vec.neg hit_rec.normal,
            ref_idx,
            ref_idx * (Vec.dot ray.dir hit_rec.normal) / (Vec.len ray.dir)
        else
            hit_rec.normal,
            1. / ref_idx,
            ~-.(Vec.dot ray.dir hit_rec.normal) / (Vec.len ray.dir)
    in
    let refracted, reflect_prob =
        match Vec.refract ray.dir out_norm ni_over_nt with
        | Some refracted -> refracted, schlick cosine ref_idx
        | None -> Vec.zero, 1.0
    in
    if randf() < reflect_prob then (
        let reflected = Vec.reflect ray.dir hit_rec.normal in
        Some (attenuation, Ray.mk hit_rec.p reflected) 
    ) else (
        Some (attenuation, Ray.mk hit_rec.p refracted)
    )
;;

let scatter ray hit_rec =
    match hit_rec.material with
    | Lambert albedo -> scatter_lambert albedo ray hit_rec
    | Metal (albedo, fuzz) -> 
            let fuzz = if fuzz < 1. then fuzz else 1. in
            scatter_metal albedo fuzz ray hit_rec
    | Dielectric ref_idx -> scatter_dielectric ref_idx ray hit_rec
;;
