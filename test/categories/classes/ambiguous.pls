class Dummy(a){}
instance Dummy(Number){}

class C(a, b) { c : (a, b) -> Number }

# TODO: fix the syntax and remove the unnecessary entailed constraint
instance (forall a. (Dummy(Number)) => C(Number, a)) {
    c(_, _) = 1
}
instance (forall a. (Dummy(Number)) => C(a, Bool)) {
    c(_, _) = 2
}

print(c(1, true))

