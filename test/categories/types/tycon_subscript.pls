# EXPECT: (5, "a")

data T = { x : Number, y : String }

let t : T = T({ x = 5, y = "a" })

print((t!.x, t!.y))
