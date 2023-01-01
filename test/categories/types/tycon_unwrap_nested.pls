# EXPECT: 5

data A = Number
data B = A

let b = B(A(5))

print(b!!)