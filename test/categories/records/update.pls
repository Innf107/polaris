# EXPECT: #{ x = 5 }

let f(r) = #{ r with x = 5 }

print(f(#{ x = 3 }))
