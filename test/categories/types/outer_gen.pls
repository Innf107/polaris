let id(x) = x

let x = ref []

let generalizeRef() = {
    x
}

generalizeRef() := ["Oh no"]

match generalizeRef()! {
    [x] -> print(x + 1)
}
