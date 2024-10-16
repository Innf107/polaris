# EXPECT: <B()>

let f : < A(< C, D >), B > -> < B >
let f(x) = match x {
    A(C) -> B
    A(D) -> B
    x -> x
}

print(f(B))