class Eq(a) {
    eq : (a, a) -> Bool
}

instance Eq(Number) {
    eq(x, y) = x == y
}

instance (forall a. (Eq(a)) => Eq(List(a))) {
    eq(list1, list2) = match (list1, list2) {
        ([], []) -> true
        (x :: xs, y :: ys) -> eq(x, y) && eq(xs, ys)
        (_, _) -> false
    }
}

print(eq([1, 2], [3, 4]))
