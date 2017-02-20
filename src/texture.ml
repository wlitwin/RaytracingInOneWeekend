open Util
open Objects

(* scatter : Ray.t -> hit_record -> (Vec.t attenuation * Ray.t scattered) option *)

let noise_texture (scale, u, v, p) =
    let open Vec in
    (0.5*(1. + sin(scale*p.z + 10.*(Perlin.turbulance (p, 7))))) *^ Vec.one
;;

let rec checker_texture (even, odd, u, v, p) =
    let open Vec in
    let sines = sin(10.*p.x) * sin(10.*p.y) * sin(10.*p.z) in
    if sines < 0. then
        texture_color (even, u, v, p)
    else
        texture_color (odd, u, v, p)

and texture_color (texture, u, v, p) =
    match texture with
    | ConstantColor color -> color
    | Checker (even, odd) -> checker_texture (even, odd, u, v, p)
    | Noise scale -> noise_texture (scale, u, v, p)
;;

let scatter_lambert (albedo, ray, hit_rec) =
    let open Ray in
    let albedo = texture_color (albedo, 0., 0., hit_rec.p) in
    let target = hit_rec.p +. hit_rec.normal +. random_in_unit_sphere() in
    let scattered = Ray.mkt hit_rec.p (target -. hit_rec.p) ray.time in
    Some (albedo, scattered)
;;

let scatter_metal (albedo, fuzz, ray, hit_rec) =
    let open Ray in
    let reflected = Vec.reflect (Vec.norm ray.dir) hit_rec.normal in
    let scattered = Ray.mkt hit_rec.p (reflected +. (Vec.s_mult fuzz (random_in_unit_sphere()))) ray.time in
    if Vec.dot scattered.dir hit_rec.normal > 0. then
        Some (albedo, scattered)
    else 
        None
;;

let scatter_dielectric (ref_idx, ray, hit_rec) =
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
        Some (attenuation, Ray.mkt hit_rec.p reflected ray.time) 
    ) else (
        Some (attenuation, Ray.mkt hit_rec.p refracted ray.time)
    )
;;

let scatter (ray , hit_rec : Ray.t * Objects.hit_record) : (Vec.t * Ray.t) option =
    match hit_rec.material with
    | Lambert albedo -> scatter_lambert (albedo, ray, hit_rec)
    | Metal (albedo, fuzz) -> 
            let fuzz = if fuzz < 1. then fuzz else 1. in
            scatter_metal (albedo, fuzz, ray, hit_rec)
    | Dielectric ref_idx -> scatter_dielectric (ref_idx, ray, hit_rec)
;;
