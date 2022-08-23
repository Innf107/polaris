# EXPECT: ["a", null]

let f() = {
    let $x = "a"
    $x
}

print([f(), $x])