# EXPECT: 5

data T = Number

let x : T = T(5)

let (T(y : Number)) = x

print(y)

