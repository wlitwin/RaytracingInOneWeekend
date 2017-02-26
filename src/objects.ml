open Vec
open Ray
open Aabb

type texture = ConstantColor of Vec.t (* color *)
             | Checker of texture (* even *) * texture (* odd *)
             | Noise of float (* scale *)
             | Image of Stb_image.int8 Stb_image.t

type material = Lambert of texture (* albedo *)
              | Metal of Vec.t (* albedo *) * float (* fuzz *)
              | Dielectric of float (* refraction index *)
              | Light of texture (* albedo *)
              | Isotropic of texture (* albedo *)

type hit_record = {
    t : float;
    u : float;
    v : float;
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

type xy_rec = {
    x0 : float;
    y0 : float;
    x1 : float;
    y1 : float;
    k  : float;
    material : material;
}

type xz_rec = {
    x0 : float;
    z0 : float;
    x1 : float;
    z1 : float;
    k  : float;
    material : material;
}

type yz_rec = {
    y0 : float;
    z0 : float;
    y1 : float;
    z1 : float;
    k  : float;
    material : material;
}

type box_rec = {
    pmin : Vec.t;
    pmax : Vec.t;
    objs : obj list;
}

and obj =
    | Sphere of Vec.t * float * material 
    | MovingSphere of mov_rec
    | BoundingVolume of Aabb.t * obj * obj
    | Box of box_rec
    | XY_Rect of xy_rec
    | XZ_Rect of xz_rec
    | YZ_Rect of yz_rec
    | Flip of obj
    | Translate of obj * Vec.t
    | Y_Rot of rot_rec
    | X_Rot of rot_rec
    | Z_Rot of rot_rec
    | ConstantMedium of material (* phase_function *) * float (* density *) * obj (* shape *)

and rot_rec = {
    sin_theta : float;
    cos_theta : float;
    obj       : obj;
    aabb      : Aabb.t;
}

let mk_box (p0, p1, material) =
    let b1 = XY_Rect {x0=p0.x; x1=p1.x; y0=p0.y; y1=p1.y; k=p1.z; material}
    and b2 = Flip (XY_Rect {x0=p0.x; x1=p1.x; y0=p0.y; y1=p1.y; k=p0.z; material})
    and b3 = XZ_Rect {x0=p0.x; x1=p1.x; z0=p0.z; z1=p1.z; k=p1.y; material}
    and b4 = Flip (XZ_Rect {x0=p0.x; x1=p1.x; z0=p0.z; z1=p1.z; k=p0.y; material})
    and b5 = YZ_Rect {y0=p0.y; y1=p1.y; z0=p0.z; z1=p1.z; k=p1.x; material}
    and b6 = Flip (YZ_Rect {y0=p0.y; y1=p1.y; z0=p0.z; z1=p1.z; k=p0.x; material}) in
    Box {pmin=p0; pmax=p1; objs=[b1;b2;b3;b4;b5;b6]}
;;

let move_sphere_center ({center0;center1;time0;time1}, time) =
    center0 +^ ((time -. time0) /. (time1 -. time0)) *^ (center1 -^ center0)
;;

let sphere_uv p =
    let phi = atan2 p.z p.x
    and theta = asin p.y in
    let u = 1. -. (phi +. Rand.pi) /. (2. *. Rand.pi)
    and v = (theta +. Rand.pi /. 2.) /. Rand.pi in
    u, v
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

let aabb_of_xy_rec {x0;y0;x1;y1;k} = {
    min = Vec.mk x0 y0 (k -. 0.0001);
    max = Vec.mk x1 y1 (k +. 0.0001);
}

let aabb_of_xz_rec {x0;z0;x1;z1;k} = {
    min = Vec.mk x0 (k -. 0.0001) z0;
    max = Vec.mk x1 (k +. 0.0001) z1;
}

let aabb_of_yz_rec {y0;z0;y1;z1;k} = {
    min = Vec.mk (k -. 0.0001) y0 z0;
    max = Vec.mk (k +. 0.0001) y1 z1;
}

let rec aabb_of_translate (offset, obj, t0, t1) =
    let aabb = aabb_of_object t0 t1 obj in
    { min = Vec.add aabb.min offset;
      max = Vec.add aabb.max offset; }

and aabb_of_object t0 t1 = function
    | Sphere (center, radius, _) -> aabb_of_sphere center radius
    | MovingSphere mov_rec -> aabb_of_moving_sphere mov_rec t0 t1
    | BoundingVolume (aabb, _, _) -> aabb
    | XY_Rect xy_rec -> aabb_of_xy_rec xy_rec
    | XZ_Rect xz_rec -> aabb_of_xz_rec xz_rec
    | YZ_Rect yz_rec -> aabb_of_yz_rec yz_rec
    | Translate (obj, offset) -> aabb_of_translate (offset, obj, t0, t1)
    | Box box_rec -> {min=box_rec.pmin; max=box_rec.pmax}
    | Flip obj -> aabb_of_object t0 t1 obj
    | ConstantMedium (_, _, obj) -> aabb_of_object t0 t1 obj
    | X_Rot {aabb}
    | Y_Rot {aabb}
    | Z_Rot {aabb} -> aabb
;;

let make_y_rot angle obj =
    let radians = (Rand.pi /. 180.) *. angle in
    let sin_theta = sin(radians)
    and cos_theta = cos(radians)
    and aabb = aabb_of_object 0. 1. obj in
    let min = ref (Vec.init max_float)
    and max = ref (Vec.init ~-.max_float) in
    for i=0 to 1 do
        for j=0 to 1 do
            for k=0 to 1 do
                let i = float i
                and j = float j
                and k = float k in
                let x = i*.aabb.max.x +. (1.-.i)*.aabb.min.x
                and y = j*.aabb.max.y +. (1.-.j)*.aabb.min.y
                and z = k*.aabb.max.z +. (1.-.k)*.aabb.min.z in
                let newx = cos_theta*.x +. sin_theta*.z
                and newz = ~-.sin_theta*.x +. cos_theta*.z in
                if newx > !max.x then begin
                    max := {!max with x=newx}
                end;
                if newx < !min.x then begin
                    min := {!min with x=newx}
                end;

                if y > !max.y then begin
                    max := {!max with y=y}
                end;
                if y < !min.y then
                    min := {!min with y=y};

                if newz > !max.z then begin
                    max := {!max with z=newz}
                end;
                if newz < !min.z then begin
                    min := {!min with z=newz}
                end;
            done;
        done;
    done;
    let aabb = Aabb.{min = !min; max = !max} in
    Y_Rot {sin_theta; cos_theta; obj; aabb}
;;

let mk_rot obj rot theta = 
    match rot with
    | `Y_Rot -> make_y_rot theta
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
;;

let hit_yz_rect ({y0;z0;y1;z1;k;material}, ray, t_min, t_max) =
    let t = (k -. ray.origin.x) /. ray.dir.x in
    if t < t_min || t > t_max then
        None
    else begin
        let y = ray.origin.y +. t*.ray.dir.y
        and z = ray.origin.z +. t*.ray.dir.z in
        if y < y0 || y > y1 || z < z0 || z > z1 then
            None
        else begin
            let u = (y -. y0) /. (y1 -. y0)
            and v = (z -. z0) /. (z1 -. z0)
            and normal = Vec.x
            and p = Ray.pos ray t in
            Some {t;u;v;p;normal;material}
        end
    end
;;

let hit_xz_rect ({x0;z0;x1;z1;k;material}, ray, t_min, t_max) =
    let t = (k -. ray.origin.y) /. ray.dir.y in
    if t < t_min || t > t_max then
        None
    else begin
        let x = ray.origin.x +. t*.ray.dir.x
        and z = ray.origin.z +. t*.ray.dir.z in
        if x < x0 || x > x1 || z < z0 || z > z1 then
            None
        else begin
            let u = (x -. x0) /. (x1 -. x0)
            and v = (z -. z0) /. (z1 -. z0)
            and normal = Vec.y
            and p = Ray.pos ray t in
            Some {t;u;v;p;normal;material}
        end
    end
;;

let hit_xy_rect ({x0;y0;x1;y1;k;material}, ray, t_min, t_max) =
    let t = (k -. ray.origin.z) /. ray.dir.z in
    if t < t_min || t > t_max then
        None
    else begin
        let x = ray.origin.x +. t*.ray.dir.x
        and y = ray.origin.y +. t*.ray.dir.y in
        if x < x0 || x > x1 || y < y0 || y > y1 then
            None
        else begin
            let u = (x -. x0) /. (x1 -. x0)
            and v = (y -. y0) /. (y1 -. y0)
            and normal = Vec.z
            and p = Ray.pos ray t in
            Some {t;u;v;p;normal;material}
        end
    end
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
            let u, v = sphere_uv (s_div radius (sub p center)) in
            Some {t;u;v;p;normal;material}
        else begin
            let temp = (-.b +. sqrt(b*.b-.a*.c)) /. a in
            if temp < t_max && temp > t_min then
                let t = temp in
                let p = Ray.pos ray t in
                let normal = s_div radius (sub p center) in
                let u, v = sphere_uv (s_div radius (sub p center)) in
                Some {t;u;v;p;normal;material}
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

and hit_y_rot (rot_rec, ray, t_min, t_max) : hit_record option =
    let cos_theta = rot_rec.cos_theta
    and sin_theta = rot_rec.sin_theta in
    let origin_x = cos_theta*.ray.origin.x -. sin_theta*.ray.origin.z
    and origin_z = sin_theta*.ray.origin.x +. cos_theta*.ray.origin.z
    and dir_x = cos_theta*.ray.dir.x -. sin_theta*.ray.dir.z
    and dir_z = sin_theta*.ray.dir.x +. cos_theta*.ray.dir.z in
    let origin = {ray.origin with x=origin_x; z=origin_z}
    and dir = {ray.dir with x=dir_x; z=dir_z} in
    let ray = {ray with origin; dir} in
    match hit (ray, t_min, t_max, rot_rec.obj) with
    | Some hit_rec ->
        let p_x = cos_theta*.hit_rec.p.x +. sin_theta*.hit_rec.p.z
        and p_z = -.sin_theta*.hit_rec.p.x +. cos_theta*.hit_rec.p.z
        and n_x = cos_theta*.hit_rec.normal.x +. sin_theta*.hit_rec.normal.z
        and n_z = -.sin_theta*.hit_rec.normal.x +. sin_theta*.hit_rec.normal.z in
        let p = {hit_rec.p with x=p_x; z=p_z}
        and normal = {hit_rec.normal with x=n_x; z=n_z} in
        Some {hit_rec with p; normal}
    | None -> None

and hit_constant_medium (material, density, obj, ray, t_min, t_max) =
    match hit (ray, ~-.max_float, max_float, obj) with
    | Some rec1 ->
        begin match hit (ray, rec1.t+.0.0001, max_float, obj) with
        | Some rec2 ->
            let t1 = max rec1.t t_min
            and t2 = min rec2.t t_max in
            if t1 >= t2 then
                None
            else begin
                let distance_inside_boundary = (t2 -. t1) *. Vec.len ray.dir in
                let hit_distance = ~-.(1. /. density) *. log(Rand.randf()) in
                if hit_distance < distance_inside_boundary then begin
                    let t = t1 +. hit_distance /. Vec.len ray.dir in
                    let p = Ray.pos ray t in
                    Some {t;p;normal=Vec.x;u=rec1.u;v=rec1.v;material}
                end else 
                    None
            end
        | None -> None
        end
    | None -> None

and hit_flip vals =
    match hit vals with
    | Some hit_rec -> Some {hit_rec with normal = Vec.neg hit_rec.normal}
    | None -> None

and hit_translate (offset, obj, ray, t_min, t_max) =
    let ray = Ray.{ray with origin=Vec.sub ray.origin offset} in
    let hit_rec = hit (ray, t_min, t_max, obj) in
    match hit_rec with
    | Some hit_rec -> Some {hit_rec with p = Vec.add hit_rec.p offset}
    | None -> None

and hit (ray, t_min, t_max, obj) = 
    match obj with
    | Sphere (center, radius, material) -> hit_sphere (center, radius, material, ray, t_min, t_max)
    | MovingSphere mov_rec -> hit_moving_sphere (mov_rec, ray, t_min, t_max)
    | BoundingVolume (aabb, obj_l, obj_r) -> hit_bounding_volume (aabb, obj_l, obj_r, ray, t_min, t_max)
    | XY_Rect xy_rec -> hit_xy_rect (xy_rec, ray, t_min, t_max)
    | XZ_Rect xz_rec -> hit_xz_rect (xz_rec, ray, t_min, t_max)
    | YZ_Rect yz_rec -> hit_yz_rect (yz_rec, ray, t_min, t_max)
    | Translate (obj, offset) -> hit_translate (offset, obj, ray, t_min, t_max)
    | Box box_rec -> hit_many (ray, t_min, t_max, box_rec.objs)
    | Flip obj -> hit_flip (ray, t_min, t_max, obj)
    | Y_Rot rot_rec -> hit_y_rot (rot_rec, ray, t_min, t_max)
    | X_Rot _ | Z_Rot _ -> failwith "X_Rot and Z_Rot hit not implemented"
    | ConstantMedium (mat, density, obj) -> hit_constant_medium (mat, density, obj, ray, t_min, t_max)

and hit_many (ray, t_min, t_max, objs) =
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
