open Vec

let perlin_generate () =
    let arr = Array.make 256 0. in
    for i=0 to 255 do
        arr.(i) <- Rand.randf()
    done;
    arr
;;

let permute (arr, n) =
    for i=n-1 downto 1 do
        let target = int_of_float (Rand.randf() *. float (i + 1)) in
        let temp = arr.(i) in
        arr.(i) <- target;
        arr.(target) <- temp;
    done;
;;

let perlin_generate_perm () =
    let arr = Array.init 256 (fun i -> i) in
    permute (arr, 256);
    arr
;;

let ranfloat = perlin_generate()
let perm_x = perlin_generate_perm()
let perm_y = perlin_generate_perm()
let perm_z = perlin_generate_perm()

(*let trilinear_interpolate*)

let noise (p : Vec.t) =
    (*
    let u = p.x -. floor p.x
    and v = p.y -. floor p.y
    and w = p.z -. floor p.z in
*)
    let i = (int_of_float (4. *. p.x)) land 255
    and j = (int_of_float (4. *. p.y)) land 255
    and k = (int_of_float (4. *. p.z)) land 255 in
    ranfloat.(perm_x.(i) lxor perm_y.(j) lxor perm_z.(k))
;;
