include Rand

type color = {
    r : float;
    g : float;
    b : float;
}

let isub = ( - )
let iadd = ( + )
let imul = ( * )
let idiv = ( / )

let ( - ) = ( -. )
let ( + ) = ( +. )
let ( * ) = ( *. )
let ( / ) = ( /. )

let ( -. ) = Vec.sub
let ( +. ) = Vec.add
let ( *. ) = Vec.mult
let ( /. ) = Vec.div

let black = { r=0.; g=0.; b=0. }
let red = { r=1.; g=0.; b=0. }
let green = { r=0.; g=1.; b=0. }
let blue = { r=0.; g=0.; b=1. }

type image = Vec.t array array

let vec_to_color {Vec.x;y;z} = {r=x;g=y;b=z}

let set_pixel image x y v =
    image.(y).(x) <- v
;;

let create_image sx sy : image =
    Array.init sy (fun _ -> (Array.make sx Vec.zero))
;;

let width image =
    Array.length image.(0)
;;

let height image =
    Array.length image
;;

let set_image f image =
    for y=isub (height image) 1 downto 0 do
        for x=0 to isub (width image) 1 do
            image.(y).(x) <- f x y 
        done
    done
;;

let iter_image f image =
    for y=isub (height image) 1 downto 0 do
        for x=0 to isub (width image) 1 do
            f x y image.(y).(x)
        done
    done
;;

let schlick cosine ref_idx =
    let r0 = (1. - ref_idx) / (1. + ref_idx) in
    let r0 = r0 * r0 in
    r0 + (1. - r0) * ((1. - cosine) ** 5.)
;;

let almost_pi = pi * 0.99999999999999999999999999999

(*
let random_in_unit_sphere () : Vec.t =
    let lambda = randf() ** 0.33333333333333333333333333333 in
    let theta = randf() * 2. * almost_pi in
    let u = randf() * 2. - 1. in
    let lamb_sqrt_u = lambda * sqrt (1. - (u * u)) in
    let open Vec in
    {x = lamb_sqrt_u * cos theta;
     y = lamb_sqrt_u * sin theta;
     z = lambda * u}
;;
*)

let rec random_in_unit_sphere () : Vec.t =
    let open Vec in
    let p = {x=2.0*randf(); y=2.0*randf(); z=2.0*randf()} -. {x=1.;y=1.;z=1.} in
    if len2 p >= 1. then
        random_in_unit_sphere()
    else
        p
;;
