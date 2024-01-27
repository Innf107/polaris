# EXPECT: 5

class Show(a){
    show : a -> String
}

instance Show(Number){
    show(x) = toString(x)
}

let existentialize : forall a. Show(a) => a -> (forall c. (forall b. Show(b) => b -> c) -> c)
let existentialize(x) = \cont -> cont(x)

existentialize(5)(\x -> print(show(x)))
