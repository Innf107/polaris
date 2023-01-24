data NumMap(a) = ()

let f : forall a. (Number, a, NumMap(a)) -> NumMap(a)
let f(key, value, map) = {
    let x = f(5)
    fail("AAA")
}
