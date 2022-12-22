export {    
    # Combining promises
    all,

    # Monadic operations
    pure,
    bind
}


let all(promises) = async [(await x) | let x <- promises]

let pure(x) = async x

# Monadic bind
let bind(promise, cont) = async {
    let x = await promise
    await cont(x)
}
