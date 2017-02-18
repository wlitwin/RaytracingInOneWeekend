type t = {
    x : float;
    y : float;
    z : float;
}

let zero = {x=0.;y=0.;z=0.}
let one = {x=1.;y=1.;z=1.}

let mk x y z = {x=x;y=y;z=z}

let op f {x;y;z} = {
    x = f x;
    y = f y;
    z = f z;
}

let neg = op ( ~-. )

let binop f {x=x1;y=y1;z=z1} {x=x2;y=y2;z=z2} = {
    x = f x1 x2;
    y = f y1 y2;
    z = f z1 z2;
}

let add = binop ( +. )

let sub v1 v2 = add v1 (neg v2)

let dot {x=x1;y=y1;z=z1} {x=x2;y=y2;z=z2} =
    x1*.x2 +.
    y1*.y2 +.
    z1*.z2
;;

let len v = sqrt (dot v v)

let len2 v = dot v v

let norm ({x;y;z} as v) =
    let l = len v in
    { x = x /. l;
      y = y /. l;
      z = z /. l; }
;;

let mult = binop ( *. )

let div = binop ( /. ) 

let s_mult s {x;y;z} = {
    x = x *. s;
    y = y *. s;
    z = z *. s;
}

let s_div s {x;y;z} = {
    x = x /. s;
    y = y /. s;
    z = z /. s;
}

let cross {x=x1;y=y1;z=z1} {x=x2;y=y2;z=z2} = {
    x = y1 *. z2 -. z1 *. y2;
    y = -.(x1 *. z2 -. z1 *. x2);
    z = x1 *. y2 -. y1 *. x2;
}

let reflect v n = sub v (s_mult (2. *. (dot v n)) n)

