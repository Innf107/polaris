# EXPECT: AAA

try {
    let _ = !sh "-c" "echo AAA; false"
} with {
    CommandFailure(out) -> print(out.stdout) 
}
