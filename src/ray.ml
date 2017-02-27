open Vec

type t = {
    origin : Vec.t;
    dir    : Vec.t;
    time   : float;
    wavelength : float;
}

(*let mk o d = {origin=o; dir=d; time=0.0}*)
let mkt o d t w = {origin=o; dir=d; time=t; wavelength=w}

let pos r t =
    Vec.add r.origin (Vec.s_mult t r.dir)
;;
