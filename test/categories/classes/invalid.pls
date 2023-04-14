
class Eq(a) {
    eq : (a, a) -> Bool
}

instance Eq(Number){
    eq(x, y) = x == y
    somethingElse(x) = x
}
