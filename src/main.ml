open Util
open Ppm

let _ =
    let nx = 200
    and ny = 100 in
    let img = create_image 200 100 in
    iter_image (fun x y _ ->
            let r = float x /. float nx
            and g = float y /. float ny
            and b = 0.2 in
            set_pixel img x y {r;g;b}
    ) img;
    write_ppm img "out.ppm"
;;
