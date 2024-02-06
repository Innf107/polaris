# EXPECT: __a__

let x = !env (\_ -> { print("a"); 0 })

print("__${x}__")
