
type ClosureRef(a) = { write : a -> (), read : () -> a }

let makeRef : forall a. a -> ClosureRef(a)
let makeRef(x) = {
    let r = ref x
    { write = \y -> r := y, read = \ -> r! }
}

let unsafeCoerce : forall a b. a -> b
let unsafeCoerce(x) = {

    let reference : forall a. ClosureRef(List(a)) = makeRef([])

    reference.write([x])

    match reference.read() {
        [y] -> y
    }
}

print(unsafeCoerce("aaa") + 1)
