# EXPECT: __a__
# KNOWN

let x = !env (\_ -> { print("a"); 0 })

print("__${x}__")
