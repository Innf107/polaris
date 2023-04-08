data ST(s, a) = a

data STRef(s, a) = Ref(a)

let newSTRef : forall s a. a -> ST (s, STRef (s, a))
let newSTRef(x) = ST(STRef(ref x))

let runST : forall a. (forall s. ST(s, a)) -> a
let runST(st) = st!


let stRef = runST(newSTRef(5))!


