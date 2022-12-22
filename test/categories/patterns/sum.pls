# EXPECT: 10

let sum(xs) = match xs {
    [] -> 0
    (x :: xs) -> x + sum(xs)
}

print(sum([1..4]))
