class Eq(a){
    eq : (a, a) -> Bool
}

instance (forall a. Eq((a, a))) {
    eq(x, y) = x == y
}

print(eq((["a"], [5]), (["a"], [5])))
