# EXPECT: 5

type T(a) = { x : a, y : List(a) }

let t : T(Number) = { x = 5, y = [6] }

let f : { x : Number, y : List(Number) } -> Number
let f(r) = r.x

print(f(t))
