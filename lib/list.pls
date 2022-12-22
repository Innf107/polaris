# Utility functions operating on lists
export {
    foldr,
    foldl,
    
    map,
    filter,

    zip,
    zipWith,
    indexed,
    
    # find,

    sum,
    product,

    for,
    forConcurrent,

    fst,
    snd,

    length,

    reverse,

    partition,
    sort,
}

# O(n)
let foldr(f, z, xs) = match xs {
    [] -> z
    (x :: xs) -> f(x, foldr(f, z, xs))
}
    

# O(n), tail recursive
let foldl(f, z, xs) = match xs {
    [] -> z
    (x :: xs) -> foldl(f, f(z, x), xs)
}

# O(length(xs))
let append(xs, ys) = foldr(\x xs -> x :: xs, ys, xs)

# O(n)
let map(f, xs) = foldr(\x r -> f(x) :: r, [], xs)

# O(n)
let filter(f, xs) = match xs {
    [] -> []
    (x :: xs) -> 
        if f(x) then
            x :: filter(f, xs)
        else
            filter(f, xs)
}

# O(min(n, m))
let zipWith(f, xs, ys) = match (xs, ys) {
    ([], _) | (_, [])-> []
    (x :: xs, y :: ys) -> f(x, y) :: zipWith(f, xs, ys)
}

# O(min(n, m))
let zip(xs, ys) = zipWith(\x y -> (x, y), xs, ys);

# O(n)
let indexed(xs) = {
    let go(ix, xs) = match xs {
        [] -> []
        (x :: xs) -> (x, ix) :: go(ix + 1, xs)
    }
    go(0, xs)
}

# Find needs Maybes which cannot be implemented right now
# O(n), tail recursive
# let find(pred, xs) = match xs {
#     [] -> null
#     x : xs -> 
#         if pred(x) then
#             x
#         else
#             find(pred, xs)
# }

# O(1)
let fst((x, y)) = x

# O(1)
let snd((x, y)) = y

# Specialized folds

# O(n), tail recursive
let sum(xs) = foldl(\r x -> r + x, 0, xs);
# O(n), tail recursive
let product(xs) = foldl(\r x -> r * x, 1, xs);


# O(n), tail recursive
let for(xs, f) = match xs {
    [] -> ()
    (x :: xs) -> {
        f(x)
        for(xs, f)
    }
}

# Evaluates each argument on a separate thread
# O(n), tail recursive
let forConcurrent(xs, f) = {
    let promises = [(async f(x)) | let x <- xs]
    for(promises, \p -> await p)
}


let length(xs) = foldl(\r _ -> r + 1, 0, xs)

let reverse(xs) = foldl(\xs x -> x :: xs, [], xs)

let partition(pred, xs) = {
    let go(passed, failed, xs) = match xs {
        [] -> (passed, failed)
        (x :: xs) -> 
            if pred(x) then
                go(x :: passed, failed, xs)
            else
                go(passed, x :: failed, xs)
    }
    go([], [], xs)
}

let sort(xs) = match xs {
    [] -> []
    # TODO: Write with let destructuring
    (x :: xs) -> let (smaller, larger) = partition(\y -> y < x, xs) in append(sort(smaller), (x :: sort(larger)))
}
