# EXPECT: { x = 3, x = "b" }

let r = { x = 1, x = "a" }

print({ r with x = 3, x = "b" })
