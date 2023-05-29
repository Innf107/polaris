# EXPECT: 5

let f(x) = match x {
    A -> B
    y -> y
}

let validate : (forall r. < A, B | r > -> < B | r >) -> ()
let validate(f) = ()

let _ = validate(f)

print(5)
