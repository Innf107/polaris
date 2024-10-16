let f : < A(Bool), B > -> < B >
let f(x) = match x {
    A(T) -> B
    A(F) -> B
    x -> x
}
