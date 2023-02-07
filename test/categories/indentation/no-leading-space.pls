# EXPECT: 5

let x = 5

let length(xs) = match xs {
    [] -> 0
    (_ :: xs) -> length(xs)
}
print(5)
