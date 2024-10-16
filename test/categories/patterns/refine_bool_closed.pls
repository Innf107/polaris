# EXPECT: <B()>

let f : < A(Bool), B > -> < B >
let f(x) = match x {
    A(true) -> B
    A(false) -> B
    x -> x
}

print(f(B))