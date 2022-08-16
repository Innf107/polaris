# EXPECT: ab

let $testtestabc = "a"

let x = !bash "-c" "echo $testtestabc"

let y = (let $testtest2 = "b" in !bash "-c" "echo $testtest2")

print(x ~ y)
