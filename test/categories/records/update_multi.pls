# EXPECT: { x = 3, x = 2 }

let r = { x = 1, x = 2 }

print({ r with x = 3, })
