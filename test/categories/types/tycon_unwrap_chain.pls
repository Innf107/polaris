# EXPECT: 5

data MyRecord = { x : Number }
data MyOtherRecord = { underlying : MyRecord }

let record = MyOtherRecord({ underlying = MyRecord({ x = 5 }) })

let x = record!.underlying!.x

print(x)