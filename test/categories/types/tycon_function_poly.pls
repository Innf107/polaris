# EXPECT: Identity(5)

data Identity(a) = a

let f = Identity

let g : forall a. a -> Identity(a) = f

print(f(5))
