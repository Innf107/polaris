# EXPECT: { x = 2, x = 3, x = 1 }

let r = { x = 1 }

print({ r extend x = 2, x = 3 })