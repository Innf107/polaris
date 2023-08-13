class Eq(a) {
    eq : (a, a) -> Bool
}

instance (forall a. Eq(a) => Eq(List(a))) {
    eq(x, y) = x == y
}

print(eq([1, 2], [3, 4]))
