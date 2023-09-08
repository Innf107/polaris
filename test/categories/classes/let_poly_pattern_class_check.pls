# EXPECT: true

class Eq(a){
    eq : (a, a) -> Bool
}
instance Eq(Number){
    eq(x, y) = x == y
}

let (g : forall a. Eq(a) => a -> Bool) : forall a. Eq(a) => a -> Bool = \x -> eq(x, x)

print(g(5))


