# EXPECT: 5

let test : () -> < >
let test() = fail("...") 

let f() = match test() {}

print(5)
