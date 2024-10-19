let f : ((< A(Bool), B >, String)) -> < B >
let f(x) = match x {
    A(true) -> B
    A(false) -> B
    x -> x
}
