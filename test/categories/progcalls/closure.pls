# EXPECT: ["a", "b"]

let argRef = ref []

!env (\args -> {argRef := args; 0}) "a" "b"

print(argRef!)
