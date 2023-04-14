# EXPECT: true

class Eq(a) {
    eq : (a, a) -> Bool
}

instance Eq(Number) {
    eq(x, y) = x == y
}

instance Eq(String) {
    eq(x, y) = x == y
}

print(eq(5, 6))
