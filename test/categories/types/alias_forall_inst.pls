
type T = forall a. a -> a


let f : T
let f(x) = {
    let g : T
    let g(y) = x
    x
}
