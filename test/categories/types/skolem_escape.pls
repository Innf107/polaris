
let f(x) = {
    let g : forall a. (a -> ()) -> ()
    let g(h) = h(x)
    x
}
