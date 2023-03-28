type Id(a) = a

let f : forall a. a -> Id(a)
let f(x) = x
