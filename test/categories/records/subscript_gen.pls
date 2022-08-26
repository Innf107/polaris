# EXPECT: 9

let f(r) = r.x + r.y

print(f(#{ x: 1, y: 2, z: 3 }) + f(#{ a: 5, y: 2, x: 4, b: 7 }))
