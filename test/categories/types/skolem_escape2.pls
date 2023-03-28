
let f(x) = {
    let i(x) = x;

    let g : forall a. (List(a) -> ()) -> ()
    let g(h) = h(i(x))
    x
}

f(5)
