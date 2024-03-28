# EXPECT: A

data A = Number
type A = String

match A(5) {
    A(5) -> print("A")
    _ -> print("B")
}
