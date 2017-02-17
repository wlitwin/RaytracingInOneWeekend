type color = {
    r : float;
    g : float;
    b : float;
}

let black = { r=0.; g=0.; b=0. }
let red = { r=1.; g=0.; b=0. }
let green = { r=0.; g=1.; b=0. }
let blue = { r=0.; g=0.; b=1. }

type image = color array array

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
    for y=height image - 1 downto 0 do
        for x=0 to width image - 1 do
            f x y image.(y).(x)
        done
    done
;;
