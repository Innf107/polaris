# EXPECT: true

class Eq(a) {
    eq : (a, a) -> Bool;
    # eq : forall a. Eq(a) => (a, a) -> Bool
}

instance (forall a. (Eq(a)) => Eq(List(a))) {
    eq(list1, list2) = match (list1, list2) {
        ([], []) -> true
        ((x :: xs), (y :: ys)) -> eq(x, y) && eq(xs, ys)
        _ -> false
    }
}

instance Eq(Number) {
    eq(x, y) = x == y
}

print(eq([[1], [2]], [[1], [2]]))
