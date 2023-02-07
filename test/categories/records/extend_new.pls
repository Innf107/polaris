# EXPECT: { x = 5, y = 3 }

let r = { y = 3 }

print({ r extend x = 5 })
