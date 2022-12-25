export {    
    # Combining promises
    all,

    # Monadic operations
    pure,
    bind
}

let all : forall a. List(Promise(a)) -> Promise(List(a))
let all(promises) = async [(await x) | let x <- promises]

let pure : forall a. a -> Promise(a)
let pure(x) = async x

# Monadic bind
let bind : forall a b. (Promise(a), a -> Promise(b)) -> Promise(b)
let bind(promise, cont) = async {
    let x = await promise
    await cont(x)
}
