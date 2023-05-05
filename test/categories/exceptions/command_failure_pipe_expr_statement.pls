# EXPECT: A

try {
    "a" | env "false"
    ()
} with {
    CommandFailure(_) -> print("A")
}
