# EXPECT: true

class Eq(a) {
    eq : (a, a) -> Bool;
    # eq : forall a. Eq(a) => (a, a) -> Bool
}

instance Eq(Number) {
    eq(x, y) = x == y
}

instance (forall a. (Eq(a)) => Eq((a, a))) {
    eq((x1, y1), (x2, y2)) = x1 == x2 && y1 == y2
}

print(eq(([5], [6]), ([5], [6])))
