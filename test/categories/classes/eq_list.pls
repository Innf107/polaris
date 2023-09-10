# EXPECT: true

class Eq(a) {
    eq : (a, a) -> Bool
}

instance (forall a. (Eq(a)) => Eq(List(a))) {
    eq(xs, ys) = match (xs, ys) {
        ([], []) -> true
        (_, []) | ([], _) -> false
        (x :: xs, y :: ys) -> x == y && xs == ys
    }
}

instance Eq(Number) {
    eq(x, y) = x == y
}

print(eq([1, 2, 3], [1, 2, 3]))
