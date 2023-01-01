# EXPECT: 5

type A = { x : Number, y : String }

let a : A = { x = 5, y = "a" }

let f : { x : Number, y : String } -> Number
let f(r) = r.x

print(f(a))
