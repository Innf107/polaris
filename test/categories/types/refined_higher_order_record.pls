

let f : forall r. { a : Number, b : Number | r } -> { b : Number | r }
let f(_) = fail("A")

let g : forall a. (a -> a) -> ()
let g(_) = ()

let _ = g(f)
