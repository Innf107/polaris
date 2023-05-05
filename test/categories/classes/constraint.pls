# EXPECT: 5

class Eq(a) {
    eq : (a, a) -> Bool
}

instance Eq(Number) {
    eq(x, y) = x == y
}

let f : Eq(a) => a -> Number
let f(x) = if eq(x, x) then 1 else 2

print(f(5))
