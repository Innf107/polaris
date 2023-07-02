# EXPECT: test 11

let x = "test ${
                let x = 5
                let y = {
                    x + 1
                }
                toString(x + y)
              }"

print(x)
