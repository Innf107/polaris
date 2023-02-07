# EXPECT: T({ x = 5, y = [1, 2, 3] })

data T(a) = { x : a, y : List(a) }

let x = T({ x = 5, y = [1, 2, 3] })

let y = T({ x = "a", y = ["b", "c"] })

print(x)
