# EXPECT: __a__

let x = !env (\_ -> { !echo "a"; 0 })

print("__${x}__")
