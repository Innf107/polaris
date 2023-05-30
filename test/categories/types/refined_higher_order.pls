

let f : forall r. < A, B | r > -> < B | r >
let f(_) = B

let g : forall a. (a -> a) -> ()
let g(_) = ()

let _ = g(f)
