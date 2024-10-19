# EXPECT: 1

let f : forall r. { x : Number, y : String | r } -> Number
let f({ y | r }) = r.x

print(f({ x = 1, y = "a"}))
