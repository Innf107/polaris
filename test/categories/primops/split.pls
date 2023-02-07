# EXPECT: (["1", " 2", "3 ", "4", ""], ["5", "6", "7", "8"], [])

let x = split(",", "1, 2,3 ,4,")
let y = split("#", "5#6#7#8")
let z = split(",", "")

print((x, y, z))
