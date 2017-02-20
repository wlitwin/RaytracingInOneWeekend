open Vec

let perlin_generate () =
    Array.init 256 (fun _ ->
        Vec.mk (-1. +. 2.*.Rand.randf())
               (-1. +. 2.*.Rand.randf())
               (-1. +. 2.*.Rand.randf())
        |> Vec.norm
    )
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

let interp_arr = Array.make 8 Vec.zero

let trilinear_interpolate (u, v, w) =
    let uu = u*.u*.(3. -. 2.*.u)
    and vv = v*.v*.(3. -. 2.*.v)
    and ww = w*.w*.(3. -. 2.*.w) in
    let accum = ref 0. in
    for i=0 to 1 do
        for j=0 to 1 do
            for k=0 to 1 do
                let idx = i*4 + j*2 + k
                and i = float i
                and j = float j
                and k = float k in
                let weight_v = Vec.mk (u -. i) (v -. j) (w -. k) in
                accum := !accum +.
                    (i*.uu +. (1.-.i)*.(1.-.uu))*.
                    (j*.vv +. (1.-.j)*.(1.-.vv))*.
                    (k*.ww +. (1.-.k)*.(1.-.ww))*.
                    (Vec.dot interp_arr.(idx) weight_v)
            done
        done
    done;
    !accum
;;

let noise (p : Vec.t) =
    let u = p.x -. floor p.x
    and v = p.y -. floor p.y
    and w = p.z -. floor p.z in
    let i = int_of_float (floor p.x)
    and j = int_of_float (floor p.y)
    and k = int_of_float (floor p.z) in
    for di=0 to 1 do
        for dj=0 to 1 do
            for dk=0 to 1 do
                let idx = di*4 + dj*2 + dk in
                interp_arr.(idx) <- ranfloat.(perm_x.((i+di) land 255) lxor
                                              perm_y.((j+dj) land 255) lxor
                                              perm_z.((k+dk) land 255));
            done
        done
    done;
    trilinear_interpolate (u, v, w)
;;

let turbulance (p, depth) =
    let rec loop (p, depth, accum, weight) =
        if depth <= 0 then accum
        else begin
            let accum = accum +. weight *. (noise p) in
            let p = Vec.s_mult 2. p in
            let weight = weight *. 0.5 in
            let depth = depth - 1 in
            loop (p, depth, accum, weight)
        end
    in
    abs_float (loop (p, depth, 0., 1.))
;;
