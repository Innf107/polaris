# EXPECT: 5

let x = `Just(5)

let f : < Just(Number), Nothing > -> Number
let f(a) = 5

print(f(x))