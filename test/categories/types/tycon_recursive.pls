# EXPECT: 5

data RoseTree(a) = { head : a, tail : List(RoseTree(a)) }

let x : RoseTree(Number) = RoseTree({ head = 5, tail = [RoseTree({ head = 6 })]})

let RoseTree(t) = x

print(t.head)
