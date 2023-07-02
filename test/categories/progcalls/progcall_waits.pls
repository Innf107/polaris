# EXPECT: (1, 2)

let r = ref 0

let _ = async {
    !sleep "0.5"
    r := 2
}
r := 1

let r1 = r!

!sleep "1"
let r2 = r!
print((r1, r2))

