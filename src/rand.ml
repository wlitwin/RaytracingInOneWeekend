let _ = Random.init 10000

let pi = 4.0 *. atan 1.0

let randf () = (*Random.float 0.99999999999999*)
    let max = 1073741824 - 1  in
    let i = Random.int max in
    float i /. float max
