# EXPECT: 5

let existentialize : forall a. List(a) -> (forall b. (forall c. List(c) -> b) -> b)
let existentialize(list) = \cont -> cont(list)

print(existentialize([1,2])(\x -> 5))
