# EXPECT: 5

type Token = < A >

let f : () -> Token
let f() = A

let g : () -> Number
let g() = match f() {
    A -> 5
}

print(g())