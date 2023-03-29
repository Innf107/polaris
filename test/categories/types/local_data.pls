# EXPECT: 5

let f(x) = {
    data A = {
        x : Number
    ,   y : Number
    }
    let f(a) = a!.x + a!.y

    f(A({x = x, y = 5}))
}

print(f(0))

