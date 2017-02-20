open Vec
open Ray
open Aabb

type texture = ConstantColor of Vec.t (* color *)
             | Checker of texture (* even *) * texture (* odd *)
             | Noise of float (* scale *)

type material = Lambert of texture (* albedo *)
              | Metal of Vec.t (* albedo *) * float (* fuzz *)
              | Dielectric of float (* refraction index *)

type hit_record = {
    t : float;
    p : Vec.t;
    normal : Vec.t;
    material : material;
}

type mov_rec = {
    center0  : Vec.t;
    center1  : Vec.t;
    time0    : float;
    time1    : float;
    radius   : float;
    material : material;
}

type obj =
    | Sphere of Vec.t * float * material 
    | MovingSphere of mov_rec
    | BoundingVolume of Aabb.t * obj * obj

let move_sphere_center ({center0;center1;time0;time1}, time) =
    center0 +^ ((time -. time0) /. (time1 -. time0)) *^ (center1 -^ center0)
;;

let aabb_of_sphere center radius : t = {
    min = center -^ Vec.init radius;
    max = center +^ Vec.init radius;
}

let aabb_of_moving_sphere mov_rec t0 t1 =
    let c0 = move_sphere_center (mov_rec, t0)
    and c1 = move_sphere_center (mov_rec, t1) in
    union (aabb_of_sphere c0 mov_rec.radius)
          (aabb_of_sphere c1 mov_rec.radius)
;;

let aabb_of_object t0 t1 = function
    | Sphere (center, radius, _) -> aabb_of_sphere center radius
    | MovingSphere mov_rec -> aabb_of_moving_sphere mov_rec t0 t1
    | BoundingVolume (aabb, _, _) -> aabb
;;

let hit_aabb (aabb, ray, t_min, t_max) : bool =
    let invD = 1.0 /. ray.dir.x in
    let t0 = (aabb.min.x -. ray.origin.x) *. invD
    and t1 = (aabb.max.x -. ray.origin.x) *. invD in
    let t0, t1 =
        if invD < 0.0 then t1, t0
        else t0, t1
    in
    let t_min = if t0 > t_min then t0 else t_min
    and t_max = if t1 < t_max then t1 else t_max in
    (*not (t_max <= t_min), t_min, t_max*)
    if t_max <= t_min then
        false
    else begin
        let invD = 1.0 /. ray.dir.y in
        let t0 = (aabb.min.y -. ray.origin.y) *. invD
        and t1 = (aabb.max.y -. ray.origin.y) *. invD in
        let t0, t1 =
            if invD < 0.0 then t1, t0
            else t0, t1
        in
        let t_min = if t0 > t_min then t0 else t_min
        and t_max = if t1 < t_max then t1 else t_max in
        if t_max <= t_min then
            false
        else begin
            let invD = 1.0 /. ray.dir.z in
            let t0 = (aabb.min.z -. ray.origin.z) *. invD
            and t1 = (aabb.max.z -. ray.origin.z) *. invD in
            let t0, t1 =
                if invD < 0.0 then t1, t0
                else t0, t1
            in
            let t_min = if t0 > t_min then t0 else t_min
            and t_max = if t1 < t_max then t1 else t_max in
            t_max > t_min
        end
    end

    (*
    let check t_min t_max sel =
        let invD = 1.0 /. (sel ray.dir) in
        let t0 = (sel aabb.min -. sel ray.origin) *. invD
        and t1 = (sel aabb.max -. sel ray.origin) *. invD in
        let t0, t1 =
            if invD < 0.0 then t1, t0
            else t0, t1
        in
        let t_min = if t0 > t_min then t0 else t_min
        and t_max = if t1 < t_max then t1 else t_max in
        not (t_max <= t_min), t_min, t_max
    in
    let rec loop t_min t_max = function
        | [sel] -> check t_min t_max sel
        | sel :: tl ->
            begin match check t_min t_max sel with
            | true, t_min, t_max -> loop t_min t_max tl
            | _false -> _false
            end
        | _ -> failwith "Not possible"
    in
    let res, _, _ = loop t_min t_max [gx;gy;gz] in
    res
    *)
;;

(* hit : (ray : Ray.t) -> (t_min : float) -> (t_max : float) -> hit_record option *)

let hit_sphere (center, radius, material, ray, t_min, t_max) : hit_record option =
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
            Some {t;p;normal;material}
        else begin
            let temp = (-.b +. sqrt(b*.b-.a*.c)) /. a in
            if temp < t_max && temp > t_min then
                let t = temp in
                let p = Ray.pos ray t in
                let normal = s_div radius (sub p center) in
                Some {t;p;normal;material}
            else 
                None
        end
    else
        None
;;

let hit_moving_sphere (mov_rec, ray, t_min, t_max) : hit_record option =
    let center = move_sphere_center (mov_rec, ray.time) in
    hit_sphere (center, mov_rec.radius, mov_rec.material, ray, t_min, t_max)
;;

let rec hit_bounding_volume (aabb, left, right, ray, t_min, t_max) : hit_record option =
    if hit_aabb (aabb, ray, t_min, t_max) then begin
        if left == right then
            hit (ray, t_min, t_max, left)
        else begin
            let left = hit (ray, t_min, t_max, left)
            and right = hit (ray, t_min, t_max, right) in
            match left, right with
            | Some left, Some right -> if left.t < right.t 
                                       then Some left
                                       else Some right
            | left, None -> left
            | None, right -> right
        end
    end else
        None

and hit (ray, t_min, t_max, obj) = 
    match obj with
    | Sphere (center, radius, material) -> hit_sphere (center, radius, material, ray, t_min, t_max)
    | MovingSphere mov_rec -> hit_moving_sphere (mov_rec, ray, t_min, t_max)
    | BoundingVolume (aabb, obj_l, obj_r) -> hit_bounding_volume (aabb, obj_l, obj_r, ray, t_min, t_max)
;;

let hit_many (ray, t_min, t_max, objs) =
    List.fold_left (fun acc obj ->
        match acc, hit (ray, t_min, t_max, obj) with
        | Some {t=t_old}, (Some {t} as out) -> if t < t_old then out else acc
        | Some _, None -> acc
        | None, s -> s
    ) None objs
;;

let rec aabb_of_list t0 t1 box lst : Aabb.t =
    let aabb_of_object = aabb_of_object t0 t1 in
    List.fold_left (fun box obj -> (union (aabb_of_object obj) box)) box lst
;;

let rand_elem lst =
    List.nth lst (Random.int (List.length lst))
;;
    
let rec build_bvh t0 t1 (lst : obj list) : obj =
    let aabbs = List.map (fun obj -> aabb_of_object t0 t1 obj, obj) lst in
    let a, o1, o2 = build_bvh_help t0 t1 aabbs in
    BoundingVolume (a, o1, o2)
and build_bvh_help (t0 : float) (t1 : float) (lst : (Aabb.t * obj) list) : (Aabb.t * obj * obj) = 
    match lst with
    | [] -> failwith "Empty!"
    | [aabb, obj] -> aabb, obj, obj
    | (aabb1, obj_1) :: (aabb2, obj_2) :: [] ->
        Aabb.union aabb1 aabb2, obj_1, obj_2
    | lst ->
        let comp_x a b = compare a.min.x b.min.x
        and comp_y a b = compare a.min.y b.min.y
        and comp_z a b = compare a.min.z b.min.z in
        let comp = rand_elem [comp_x;comp_y;comp_z] in
        let lst = List.sort (fun (aabb1, _) (aabb2, _) -> comp aabb1 aabb2) lst in
        let mid = List.length lst / 2
        and count = ref 0 in
        let left, right = List.partition (fun _ -> incr count; !count <= mid) lst in
        let (aabb1, o11, o12), (aabb2, o21, o22) =
            build_bvh_help t0 t1 left, 
            build_bvh_help t0 t1 right in
        (Aabb.union aabb1 aabb2, BoundingVolume (aabb1, o11, o12), BoundingVolume (aabb2, o21, o22))
;;
