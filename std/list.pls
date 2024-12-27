# Utility functions operating on lists
export {
    foldr,
    foldl,

    append,

    map,
    concatMap,
    filter,
    filterMap,

    zip,
    zipWith,
    indexed,
    
    contains,
    find,

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
let foldr : forall a b. ((a, b) -> b, b, List(a)) -> b
let foldr(f, z, xs) = match xs {
    [] -> z
    (x :: xs) -> f(x, foldr(f, z, xs))
}
    

# O(n), tail recursive
let foldl : forall a b. ((b, a) -> b, b, List(a)) -> b
let foldl(f, z, xs) = match xs {
    [] -> z
    (x :: xs) -> foldl(f, f(z, x), xs)
}

# O(length(xs))
let append : forall a. (List(a), List(a)) -> List(a)
let append(xs, ys) = foldr(\x xs -> x :: xs, ys, xs)

# O(n)
let map : forall a b. (a -> b, List(a)) -> List(b)
let map(f, xs) = foldr(\x r -> f(x) :: r, [], xs)

let concatMap : forall a b. (a -> List(b), List(a)) -> List(b)
let concatMap(f, xs) = foldr(\x r -> append(f(x), r), [], xs)

# O(n)
let filter : forall a. (a -> Bool, List(a)) -> List(a)
let filter(f, xs) = match xs {
    [] -> []
    (x :: xs) -> 
        if f(x) then
            x :: filter(f, xs)
        else
            filter(f, xs)
}

# O(n)
let filterMap : forall a b. (a -> < Just(b), Nothing > , List(a)) -> List(b)
let filterMap(f, xs) = match xs {
    [] -> []
    (x :: xs) -> match f(x) {
            Nothing -> filterMap(f, xs)
            Just(y) -> y :: filterMap(f, xs)
        }
}

# O(min(n, m))
let zipWith : forall a b c. ((a, b) -> c, List(a), List(b)) -> List(c)
let zipWith(f, xs, ys) = match (xs, ys) {
    ([], _) | (_, [])-> []
    (x :: xs, y :: ys) -> f(x, y) :: zipWith(f, xs, ys)
}

# O(min(n, m))
let zip : forall a b. (List(a), List(b)) -> List((a, b))
let zip(xs, ys) = zipWith(\x y -> (x, y), xs, ys);

# O(n)
let indexed : forall a. List(a) -> List((a, Number))
let indexed(xs) = {
    let go(ix, xs) = match xs {
        [] -> []
        (x :: xs) -> (x, ix) :: go(ix + 1, xs)
    }
    go(0, xs)
}

# O(n), tail recursive
let contains : forall a. (a, List(a)) -> Bool
let contains(needle, haystack) = match haystack {
    [] -> false
    (value :: rest) -> if value == needle then true else contains(needle, rest)
}

# O(n), tail recursive
let find : forall a. ((a -> Bool), List(a)) -> < Just(a), Nothing >
let find(pred, xs) = match xs {
    [] -> Nothing
    (x :: xs) -> 
        if pred(x) then
            Just(x)
        else
            find(pred, xs)
    
}

# O(1)
let fst : forall a b. ((a, b)) -> a
let fst((x, y)) = x

# O(1)
let snd : forall a b. ((a, b)) -> b
let snd((x, y)) = y

# Specialized folds

# O(n), tail recursive
let sum : List(Number) -> Number
let sum(xs) = foldl(\r x -> r + x, 0, xs);
# O(n), tail recursive
let product : List(Number) -> Number
let product(xs) = foldl(\r x -> r * x, 1, xs);


# O(n), tail recursive
let for : forall a. (List(a), a -> ()) -> ()
let for(xs, f) = match xs {
    [] -> ()
    (x :: xs) -> {
        f(x)
        for(xs, f)
    }
}

# Evaluates each argument on a separate thread
# O(n), technically tail recursive, but it is probably not a good idea to run this on a large list
let forConcurrent : forall a. (List(a), a -> ()) -> ()
let forConcurrent(xs, f) = {
    let promises = [(async f(x)) | let x <- xs]
    for(promises, \p -> await p)
}

# O(n), tail recursive
let length : forall a. List(a) -> Number
let length(xs) = foldl(\r _ -> r + 1, 0, xs)

# O(n), tail recursive
let reverse : forall a. List(a) -> List(a)
let reverse(xs) = foldl(\xs x -> x :: xs, [], xs)

# O(n), tail recursive
let partition : forall a. (a -> Bool, List(a)) -> (List(a), List(a))
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

let sort : List(Number) -> List(Number)
let sort(xs) = match xs {
    [] -> []
    (x :: xs) -> { 
        let (smaller, larger) = partition(\y -> y < x, xs)
        append(sort(smaller), (x :: sort(larger)))
    }
}
