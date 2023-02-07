# EXPECT: { x = 5, x = 3 }

let f(r) = { r extend x = 5 }

print(f({ x = 3 }))
