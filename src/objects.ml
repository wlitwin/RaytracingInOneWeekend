open Vec
open Ray

type hit_record = {
    t : float;
    p : Vec.t;
    normal : Vec.t;
}

type obj =
    | Sphere of Vec.t * float

(* hit : (ray : Ray.t) -> (t_min : float) -> (t_max : float) -> hit_record option *)

let hit_sphere center radius ray t_min t_max : hit_record option =
    let oc = sub ray.origin center in
    let a = dot ray.dir ray.dir in
    let b = dot oc ray.dir in
    let c = (dot oc oc) -. (radius *. radius) in
    let descriminant = b*.b -. a*.c in
    if descriminant > 0. then
        let temp = (-.b -. sqrt(b*.b -. a*.c)) /. a in
        if temp < t_max && temp > t_min then
            let t = temp in
            let p = Ray.pos ray t in
            let normal = s_div radius (sub p center) in
            Some {t;p;normal}
        else begin
            let temp = (-.b +. sqrt(b*.b-.a*.c)) /. a in
            if temp < t_max && temp > t_min then
                let t = temp in
                let p = Ray.pos ray t in
                let normal = s_div radius (sub p center) in
                Some {t;p;normal}
            else 
                None
        end
    else
        None
;;

let hit ray t_min t_max = function
    | Sphere (center, radius) -> hit_sphere center radius ray t_min t_max
;;

let hit_many ray t_min t_max objs =
    List.fold_left (fun acc obj ->
        match acc, hit ray t_min t_max obj with
        | Some {t=t_old}, (Some {t} as out) -> if t < t_old then out else acc
        | Some _, None -> acc
        | None, s -> s
    ) None objs
;;
