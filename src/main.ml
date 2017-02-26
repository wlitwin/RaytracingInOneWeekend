open Helpers
open Ppm_Utils
open Vec
open Ray
open Objects
open Texture

let nx = 800
let ny = 400
let ns = 6000

let fnx = float nx
let fny = float ny
let fsamps = float ns

let rec shoot_ray (ray, objs, depth) : Vec.t =
    match hit_many (ray, 0.001, max_float, objs) with
    | Some hit_rec -> 
            let light = emitted hit_rec in
            if depth < 50 then
                match scatter (ray, hit_rec) with
                | Some (attenuation, scattered) ->
                    light +. attenuation *. (shoot_ray (scattered, objs, (iadd depth 1)))
                | None -> light
            else
                light
    | None ->
        Vec.zero
            (*
        let n_dir = norm ray.dir in
        let t : float = 0.5 * (n_dir.y + 1.0) in
        accum_atten *. (add (s_mult (1.0 - t) Vec.one) (s_mult t {x=0.5;y=0.7;z=1.0}))
*)
;;

let sample_ray (camera, objs, samps) (x, y) =
    let rec loop i color = 
        match i with
        | 0 -> color
        | n ->
            let u = (x + randf()) / fnx
            and v = (y + randf()) / fny in
            let ray = Camera.get_ray (camera, u, v) in
            loop (isub n 1) (color +. shoot_ray (ray, objs, 0))
    in
    let color = loop samps Vec.zero in
    s_div fsamps color
;;

let rand_color() =
    Vec.mk (randf() * 0.8 + 0.2) (randf() * 0.8 + 0.2) (randf() * 0.8 + 0.2)
;;

let load_image filename =
    let open Stb_image in
    match load ~channels:3 filename with
    | Ok img -> img
    | Error (`Msg msg) -> failwith msg
;;

let rand_scene () =
    (*
    let image = Image (load_image "pics/large.png") in
    let checker = Checker (ConstantColor (Vec.mk 0.1 0.1 0.1),
                           ConstantColor (Vec.mk 0.9 0.9 0.9)) in
    let s1 = Sphere (Vec.mk 0. ~-.1000. 0., 1000., Lambert (Noise 5.))
    and s2 = Sphere ({x=0.;y= 2.;z= 0.}, 2., Lambert (Noise 5.))
    and s4 = Sphere ({x= 0.0;y= 7.;z= 0.}, 2., Light (ConstantColor (Vec.init 2.)))
    and r1 = XY_Rect {x0=3.;x1=5.;y0=1.;y1=3.;k= ~-.2.;material=Light (ConstantColor (Vec.mk 4. 0. 0.))} in
    let rec accume lst count =
        if count <= 0 then lst
        else
            let mat = match randf() with
                    | x when x < 0.33 -> Lambert (ConstantColor (rand_color()))
                    | x when x < 0.66 -> Metal (rand_color(), randf())
                    | x -> Dielectric (randf() * 4.)
            in
            (*
            let lst = Sphere (Vec.mk (randf() * 10. - 5.)
                             (randf() * 0.125 + 0.125)
                             (randf() * 10. - 5.),
                             (randf() * 0.15 + 0.05), 
                             mat) 
                             *)
            let lst = 
                let center0 = Vec.mk (randf() * 10. - 5.)
                                     (randf() * 0.125 + 0.125)
                                     (randf() * 10. - 5.)
                in
                MovingSphere {
                    center0;
                    center1=center0 +^ Vec.mk 0. 0.5 0.;
                    time0=0.;
                    time1=1.;
                    radius=randf() * 0.15 + 0.05;
                    material=mat;
                }
                      :: lst in
            accume lst (isub count 1)
    in
    [build_bvh 0. 1. (accume [s1;s2;s4;r1] 1)]
*)
    let red = Lambert (ConstantColor (Vec.mk 0.65 0.05 0.05))
    and white = Lambert (ConstantColor (Vec.init 0.73))
    and green = Lambert (ConstantColor (Vec.mk 0.12 0.45 0.15))
    and light = Light (ConstantColor (Vec.init 15.))
    and smoke1 = Isotropic (ConstantColor (Vec.init 1.))
    and smoke2 = Isotropic (ConstantColor (Vec.init 0.)) in
    let r1 = Flip (YZ_Rect {y0=0.;y1=555.;z0=0.;z1=555.;k=555.;material=green})
    and r2 = YZ_Rect {y0=0.;y1=555.;z0=0.;z1=555.;k=0.;material=red}
    and r3 = XZ_Rect {x0=213.;x1=343.;z0=227.;z1=332.;k=554.;material=light}
    and r4 = Flip (XZ_Rect {x0=0.;x1=555.;z0=0.;z1=555.;k=555.;material=white})
    and r5 = XZ_Rect {x0=0.;x1=555.;z0=0.;z1=555.;k=0.;material=white}
    and r6 = Flip (XY_Rect {x0=0.;x1=555.;y0=0.;y1=555.;k=555.;material=white})
    and b1 = mk_box (Vec.mk 0. 0. 0., Vec.mk 165. 165. 165., white)
    and b2 = mk_box (Vec.mk 0. 0. 0., Vec.mk 165. 330. 165., white) in
    let b1 = Translate (make_y_rot ~-.18. b1, Vec.mk 130. 0. 65.)
    and b2 = Translate (make_y_rot   15. b2, Vec.mk 265. 0. 295.) in
    let b1 = ConstantMedium (smoke1, 0.01, b1)
    and b2 = ConstantMedium (smoke2, 0.01, b2) in
    [build_bvh 0. 1. [r1;r2;r3;r4;r5;r6;b1;b2]]
;;
    
let objs = rand_scene()
let from = Vec.mk 278. 278. ~-.800.
let _to  = Vec.mk 278. 278. 0.
let dist_to_focus = 10.
let fov = 40.
let aperature = 0.
let camera = Camera.look_at from _to Vec.y fov (float nx / float ny) aperature dist_to_focus 0. 0.
let sample_ray = sample_ray (camera, objs, ns)

let trace_image start_y stride length chan id =
    let img = create_image nx length in
    (*
    for i=0 to isub length 1 do
        Printf.printf "id %d covering %d\n" id (calc_offset i);
        flush stdout
    done;
    *)
    let calc_offset i = iadd start_y (imul i stride) in
    set_image (fun x y ->
        let x = float x
        and y = float (calc_offset y) in
        sample_ray (x, y) |> op sqrt
    ) img;
    Printf.printf "%d dumping data\n" id;
    flush stdout;
    iter_image (fun x y pixel ->
        output_binary_int chan x; 
        output_binary_int chan (calc_offset y); 
        output_value chan pixel;
    ) img;
    Printf.printf "%d done closing\n" id;
    flush stdout;
    flush chan;
    close_out chan;
;;

let read_image_from_children chans =
    let img = create_image nx ny in
    let rec read_chan chan = 
        try 
            let x = input_binary_int chan
            and y = input_binary_int chan
            and v = input_value chan in
            set_pixel img x y v;
            read_chan chan
        with _ -> 
            close_in chan;
            false
    in
    let rec loop = function
        | [] -> ()
        | chans ->
            chans
            |> List.map (fun chan -> (chan, read_chan chan))
            |> List.filter snd
            |> List.map fst
            |> loop
    in
    loop chans;
    write_ppm img "out.ppm"
;;

let _stdout = stdout

let spawn_processes num =
    let open Unix in
    let num = if num > ny then ny else num in
    let len = idiv ny num in
    let stride = num in
    let rem = ny mod stride in
    Printf.printf "About to spawn\n";
    flush _stdout;
    let rec spawn_it num offset fids =
        if num <= 0 then (
            Printf.printf "Reading mode\n";
            flush _stdout;
            (* Switch to reading mode *)
            read_image_from_children fids
        ) else begin
            let p_to_ch_in, p_to_ch_out = pipe() in
            let ch_to_p_in, ch_to_p_out = pipe() in
            match fork() with
            | x when x < 0 -> Printf.printf "Fork failed\n"; flush _stdout
            | 0 ->
                Printf.printf "Spawning %d\n" num;
                flush _stdout;
                close p_to_ch_in;
                close ch_to_p_out;
                let chan = out_channel_of_descr p_to_ch_out in
                let in_chan = in_channel_of_descr ch_to_p_in in
                let len = if offset < rem then iadd len 1 else len in
                output_binary_int chan offset;
                output_binary_int chan stride;
                output_binary_int chan num;
                output_binary_int chan len;
                close_out chan;
                spawn_it (isub num 1) (iadd offset 1) (in_chan :: fids)
            | child ->
                close p_to_ch_out;
                close ch_to_p_in;
                let chan = in_channel_of_descr p_to_ch_in in
                let out_chan = out_channel_of_descr ch_to_p_out in
                let offset = input_binary_int chan
                and stride = input_binary_int chan
                and id = input_binary_int chan
                and len = input_binary_int chan in
                close_in chan;
                Printf.printf "offset %d length %d id %d\n" offset len id;
                flush _stdout;
                trace_image offset stride len out_chan id;
        end
    in
    spawn_it num 0 []
;;

let single_threaded () =
    let img = create_image nx ny in
    set_image (fun x y ->
        let x = float x
        and y = float y in
        sample_ray (x, y) |> op sqrt
    ) img;
    write_ppm img "out.ppm"
;;

(*
let gc_settings = 
let open Gc in    
{
    minor_heap_size = 1000000;
    major_heap_increment = 1000;
    space_overhead = 90;
    verbose = 0;
    max_overhead = 1000; (* never triggered *)
    stack_limit = 4096000;
    allocation_policy = 1;
    window_size = 1;
}
*)

let _ =
    let default_procs = 4 in
    let num_procs =
        if Array.length Sys.argv > 1 then
            try
                int_of_string Sys.argv.(1)
            with _ -> default_procs
        else default_procs
    in
    if num_procs > 0 then
        spawn_processes num_procs
    else
        single_threaded ()
