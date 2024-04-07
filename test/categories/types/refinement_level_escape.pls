# EXPECT: T(<A>)
# KNOWN

data T(r) = < A, B | r >

let cast : forall r. T(< >) -> T(r)
let cast(x) = T(match x! {
    A -> A
    B -> B
})

print(cast(T(A)) : T(< C >))
