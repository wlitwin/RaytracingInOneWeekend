open Vec
open Ray

let hit_sphere center radius ray =
    let oc = sub ray.origin center in
    let a = dot ray.dir ray.dir in
    let b = 2.0 *. dot oc ray.dir in
    let c = (dot oc oc) -. (radius *. radius) in
    let descriminant = b*.b -. 4.0*.a*.c in
    descriminant > 0.
;;
