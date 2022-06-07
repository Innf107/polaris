# Utility functions operating on lists


# O(n)
let foldr(f, z, xs) = 
    if xs == [] then
        z
    else
        f(head(xs), foldr(f, z, tail(xs)));

# O(n), tail recursive
let foldl(f, z, xs) =
    if xs == [] then
        z
    else
        foldl(f, f(z, head(xs)), tail(xs));

# O(n)
let map(f, xs) = foldr(\(x, r) -> cons(f(x), r), [], xs);

# O(n)
let filter(f, xs) =
    if xs == [] then
        []
    else
        if f(head(xs)) then
            cons(head(xs), filter(f, tail(xs)))
        else
            filter(f, tail(xs));

# O(min(n, m))
let zipWith(f, xs, ys) = {
    if xs == [] then
        []
    else if ys == [] then
        []
    else 
        cons(f(head(xs), head(ys)), zipWith(f, tail(xs), tail(ys)))
};

# O(min(n, m))
let zip(xs, ys) = zipWith(\(x, y) -> [x, y], xs, ys);

# Returns the first item `x` in the list, such that `pred(x) = true`
# or `null`, if no such item exists.
# O(n), tail recursive
let find(pred, xs) =
    if xs == [] then
        null
    else if pred(head(xs)) then
        head(xs)
    else
        find(pred, tail(xs));

# O(1)
let fst(t) = head(t);

# O(1)
let snd(t) = head(tail(t));

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
let for(xs, f) = {
    if xs == [] then
        ()
    else {
        f(head(xs));
        for(tail(xs), f)
    }
};

# Evaluates each argument on a separate thread
# O(n), tail recursive
let forConcurrent(xs, f) = {
    let promises = [(async f(x)) | x <- xs];
    for(promises, \p -> await p);
};

let fst(t) = head(t);

let snd(t) = head(tail(t));


let length(xs) = match xs {
    [] -> 0
    (_ : xs) -> length(xs)
}

#{
    foldr: foldr,
    foldl: foldl,
    
    map: map,
    filter: filter,

    zip: zip,
    zipWith: zipWith,

    find: find,
    lookupWith: lookupWith,
    lookup: lookup,
    
    sum: sum,
    product: product,

    for: for,
    forConcurrent: forConcurrent,

    fst: fst,
    snd: snd,

    length: length
}

