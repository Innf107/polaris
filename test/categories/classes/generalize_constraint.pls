# EXPECT: (true, true)

class Eq(a){
    eq : (a, a) -> Bool
}

instance Eq(Number){
    eq(x, y) = x == y
}

instance Eq(String){
    eq(x, y) = x == y
}

let f(x) = eq(x, x)

print((f(5), f("A")))