open Util

let write_ppm (image : image) (file : string) : unit =
    let chan = open_out file in
    Printf.fprintf chan "P3\n%d %d\n255\n" (width image) (height image);
    iter_image (fun x y pixel ->
        let r = int_of_float (pixel.r * 255.99)
        and g = int_of_float (pixel.g * 255.99)
        and b = int_of_float (pixel.b * 255.99) in
        Printf.fprintf chan "%d %d %d\n" r g b
    ) image;
    close_out chan
;;
