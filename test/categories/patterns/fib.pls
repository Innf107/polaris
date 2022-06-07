# EXPECT: 89

let fib(n) = match n {
    0 | 1 -> 1
    n -> fib(n - 1) + fib(n - 2)
}

print(fib(10))