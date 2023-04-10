data ST(s, a) = a

data STRef(s, a) = Ref(a)

let newSTRef : forall s a. a -> ST (s, STRef (s, a))
let newSTRef(x) = ST(STRef(ref x))

let runST : forall a. (forall s. ST(s, a)) -> a
let runST(st) = st!

let readSTRef : forall s a. STRef(s, a) -> ST(s, a)
let readSTRef(STRef(r)) = ST(r!)

let bind : forall s a b. (ST(s, a), a -> ST(s, b)) -> ST(s, b)
let bind(ST(x), cont) = cont(x)

let pure : forall s a. a -> ST(s, a)
let pure(x) = ST(x)

let stRef = runST(bind(newSTRef(5), \r -> 
                  bind(readSTRef(r), \x ->
                  pure(ref x))))!


