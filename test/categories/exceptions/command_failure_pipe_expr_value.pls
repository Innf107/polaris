# EXPECT: A

try {
    let _ = "a" | env "false"
} with {
    CommandFailure(_) -> print("A")
}
