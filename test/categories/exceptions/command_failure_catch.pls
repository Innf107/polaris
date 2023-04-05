# EXPECT: Failed command: 'false' called with arguments: ["something", "something else"]

try {
    let _ = !false "something" "something else" 
} with {
    CommandFailure(result) -> print("Failed command: '" ~ result.program ~ "' called with arguments: " ~ toString(result.arguments)) 
}


