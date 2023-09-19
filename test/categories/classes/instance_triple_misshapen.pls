class C(a, b) { c : (a, b) -> Number }

instance (forall a. C(Number, a)) {
    c(_, _) = 1
}
instance (forall a. C(a, Bool)) {
    c(_, _) = 2
}

instance C(Number, List(Bool)) { c(_, _) = 3 }

print(c(1, true))

