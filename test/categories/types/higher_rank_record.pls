let f : { g : forall a. a -> a } -> (Number, String)
let f(r) = (r.g(5), r.g("a"))
