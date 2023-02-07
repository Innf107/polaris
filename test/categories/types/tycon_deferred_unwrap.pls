# EXPECT: 5

data A = Number

let f : A -> ()
let f(_) = ()

if false then {
    let a = fail("this might return anything")

    print(a!)

    f(a)
} else {}

print(5)

