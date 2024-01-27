# EXPECT: 5

class Show(a){
    show : a -> String
}

instance Show(Number){
    show(x) = toString(x)
}

let f : () -> (forall a. Show(a) => a -> String)
let f() = show

print(f()(5))

