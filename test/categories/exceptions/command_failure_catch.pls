# EXPECT: Failed command: 'false' called with arguments: ["something", "something else"]

try {
    let _ = !false "something" "something else" 
} with {
    CommandFailure(command, args) -> print("Failed command: '" ~ command ~ "' called with arguments: " ~ toString(args)) 
}


