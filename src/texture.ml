open Helpers
open Objects

(* scatter : Ray.t -> hit_record -> (Vec.t attenuation * Ray.t scattered) option *)

let noise_texture (scale, u, v, p) =
    let open Vec in
    (0.5*(1. + sin(scale*p.z + 10.*(Perlin.turbulance (p, 7))))) *^ Vec.one
;;

let clamp (v, low, high) =
    (min (max v low) high)
;;

let image_texture (image, u, v, p) =
    let open Stb_image in
    let w, h = image.width, image.height in
    let i = int_of_float (u * float w)
    and j = int_of_float ((1. - v) * float h - 0.001) in
    let i = clamp (i, 0, isub w 1)
    and j = clamp (j, 0, isub h 1)
    and ( * ) = imul
    and ( + ) = iadd in
    let idx = 3*i +3*image.width*j in
    let r = (float image.data.{idx + 0}) / 255.0
    and g = (float image.data.{idx + 1}) / 255.0
    and b = (float image.data.{idx + 2}) / 255.0 in
    Vec.mk r g b
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
    | Image image -> image_texture (image, u, v, p)
;;

let scatter_lambert (albedo, ray, hit_rec) =
    let open Ray in
    let albedo = texture_color (albedo, hit_rec.u, hit_rec.v, hit_rec.p) in
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

let emitted (hit_rec : Objects.hit_record) =
    match hit_rec.material with
    | Light albedo -> texture_color (albedo, hit_rec.u, hit_rec.v, hit_rec.p)
    | _ -> Vec.zero
;;

let scatter (ray , hit_rec : Ray.t * Objects.hit_record) : (Vec.t * Ray.t) option =
    match hit_rec.material with
    | Lambert albedo -> scatter_lambert (albedo, ray, hit_rec)
    | Metal (albedo, fuzz) -> 
            let fuzz = if fuzz < 1. then fuzz else 1. in
            scatter_metal (albedo, fuzz, ray, hit_rec)
    | Dielectric ref_idx -> scatter_dielectric (ref_idx, ray, hit_rec)
    | Light _ -> None
;;
