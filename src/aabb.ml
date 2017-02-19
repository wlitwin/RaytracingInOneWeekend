type t = {
    min : Vec.t;
    max : Vec.t;
}

let zero = {
    min = Vec.zero;
    max = Vec.zero;
}

let union box1 box2 = {
    min = Vec.binop min box1.min box2.min;
    max = Vec.binop max box1.max box2.max;
}

