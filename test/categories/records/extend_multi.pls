# EXPECT: { x = 3, x = "b", x = 1, x = "a" }

let r = { x = 1, x = "a" }

print({ r extend x = 3, x = "b" })
