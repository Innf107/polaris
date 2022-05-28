
let all(promises) = async [(await x) | x <- promises];

let pure(x) = async x;

# Monadic bind
let bind(promise, cont) = async {
    let x = await promise;
    await cont(x);
};

#{
    # Combining promises
    all: all,

    # Monadic operations
    pure: pure,
    bind: bind,
}