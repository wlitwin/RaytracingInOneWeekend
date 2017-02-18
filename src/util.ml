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

type image = color array array

let vec_to_color {Vec.x;y;z} = {r=x;g=y;b=z}

let set_pixel image x y v =
    image.(y).(x) <- v
;;

let create_image sx sy : image =
    Array.init sy (fun _ -> (Array.make sx black))
;;

let width image =
    Array.length image.(0)
;;

let height image =
    Array.length image
;;

let iter_image f image =
    for y=isub (height image) 1 downto 0 do
        for x=0 to isub (width image) 1 do
            f x y image.(y).(x)
        done
    done
;;

let randf () = Random.float 0.99999999999999

let rec random_in_unit_sphere () =
    let open Vec in
    let p = {x=2.0*randf(); y=2.0*randf(); z=2.0*randf()} -. {x=1.;y=1.;z=1.} in
    if len2 p >= 1. then
        random_in_unit_sphere()
    else
        p
;;
