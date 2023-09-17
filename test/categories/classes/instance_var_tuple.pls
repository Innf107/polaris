# EXPECT: 1

class C(a, b) { c : (a, b) -> Number }

instance (forall a. C(Number, a)) {
    c(_, _) = 1
}

print(c(1, true))

