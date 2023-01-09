# EXPECT: 5

let f : forall a r. { x : a | r } -> a
let f(r) = r.x

print(f({ x = 5, y = "a" }))