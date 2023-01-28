# EXPECT: ["a", ""]

let f() = {
    let $x = "a"
    $x
}

print([f(), $x])