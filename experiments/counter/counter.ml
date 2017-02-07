type t = int

let inc t x = t + x
let dec t x = t - x

let merge old v1 v2 = old + (v1-old) + (v2-old)
