# Utility functions operating on lists


# O(n)
let foldr(f, z, xs) = match xs {
    [] -> z
    (x : xs) -> f(x, foldr(f, z, xs))
}
    

# O(n), tail recursive
let foldl(f, z, xs) = match xs {
    [] -> z
    (x : xs) -> foldl(f, f(z, x), xs)
}

# O(n)
let map(f, xs) = foldr(\(x, r) -> [f(x)] ~ r, [], xs)

# O(n)
let filter(f, xs) = match xs {
    [] -> []
    (x : xs) -> 
        if f(x) then
            [x] ~ filter(f, xs)
        else
            filter(f, xs)
}

# O(min(n, m))
let zipWith(f, xs, ys) = match [xs, ys] {
    [[], _] | [_, []]-> []
    [x : xs, y : ys] -> [f(x, y)] ~ zipWith(f, xs, ys)
}

# O(min(n, m))
let zip(xs, ys) = zipWith(\(x, y) -> [x, y], xs, ys);

# O(n)
let indexed(xs) = {
    let go(ix, xs) = match xs {
        [] -> []
        (x : xs) -> [[x, ix]] ~ go(ix + 1, xs)
    }
    go(0, xs)
}

# Returns the first item `x` in the list, such that `pred(x) = true`
# or `null`, if no such item exists.
# O(n), tail recursive
let find(pred, xs) = match xs {
    [] -> null
    x : xs -> 
        if pred(x) then
            x
        else
            find(pred, xs)
}

# O(1)
let fst(t) = head(t)

# O(1)
let snd(t) = head(tail(t))

# Takes a list of pair and retuns the second element of the first
# pair `p`, where `pred(fst(p)) = true` or null, if no such pair exists.
# O(n), tail recursive
let lookupWith(pred, xs) =
    let result = find(\t -> pred(fst(t)), xs) in
    if result == null then
        null
    else
        snd(result);

# O(n), tail recursive
let lookup(k, xs) = lookupWith(\x -> x == k, xs);

# Specialized folds

# O(n), tail recursive
let sum(xs) = foldl(\(r, x) -> r + x, 0, xs);
# O(n), tail recursive
let product(xs) = foldl(\(r, x) -> r * x, 1, xs);


# O(n), tail recursive
let for(xs, f) = match xs {
    [] -> ()
    (x : xs) -> {
        f(x)
        for(xs, f)
    }
}

# Evaluates each argument on a separate thread
# O(n), tail recursive
let forConcurrent(xs, f) = {
    let promises = [(async f(x)) | x <- xs]
    for(promises, \p -> await p)
}


let length(xs) = match xs {
    [] -> 0
    (_ : xs) -> length(xs)
}

let reverse(xs) = foldl(\(xs, x) -> cons(x, xs), [], xs)

let partition(pred, xs) = {
    let go(passed, failed, xs) = match xs {
        [] -> [passed, failed]
        (x : xs) -> 
            if pred(x) then
                go([x] ~ passed, failed, xs)
            else
                go(passed, [x] ~ failed, xs)
    }
    go([], [], xs)
}

let sort(xs) = match xs {
    [] -> []
    # TODO: Write with let destructuring
    (x : xs) -> match partition(\y -> y < x, xs) {
        [smaller, larger] -> sort(smaller) ~ [x] ~ sort(larger)
    }
}

#{
    foldr: foldr,
    foldl: foldl,
    
    map: map,
    filter: filter,

    zip: zip,
    zipWith: zipWith,
    indexed: indexed,

    find: find,
    lookupWith: lookupWith,
    lookup: lookup,
    
    sum: sum,
    product: product,

    for: for,
    forConcurrent: forConcurrent,

    fst: fst,
    snd: snd,

    length: length,

    reverse: reverse,

    partition: partition,
    sort: sort,
}

