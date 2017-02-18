open Vec

type t = {
    origin : Vec.t;
    dir    : Vec.t;
}

let mk o d = {origin=o; dir=d}

let pos r t =
    Vec.add r.origin (Vec.s_mult t r.dir)
;;
