class Num(a) {
    fromInt : Number -> a
    add : (a, a) -> a
    multiply : (a, a) -> a
}

instance Num(Number) {
    fromInt(x) = x
}

