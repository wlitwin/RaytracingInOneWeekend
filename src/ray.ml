open Vec

type channel = Red
             | Green
             | Blue
             | All

type t = {
    origin : Vec.t;
    dir    : Vec.t;
    time   : float;
}

type split = SingleRay of t
           | MultiRay of t * t * t (* Red Green Blue *) 

(*let mk o d = {origin=o; dir=d; time=0.0}*)
let mkt o d t = {origin=o; dir=d; time=t}

let pos r t =
    Vec.add r.origin (Vec.s_mult t r.dir)
;;
