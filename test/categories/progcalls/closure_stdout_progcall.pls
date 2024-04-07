# EXPECT: __a__
# KNOWN

let x = !env (\_ -> { !echo "a"; 0 })

print("__${x}__")
