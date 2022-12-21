# System functions
export {
    rand,
    randomInt
}

# Generate an arbitrary random integer.
# This is exactly equivalent to bash's $RANDOM (because that is how it is implemented)
let rand() = parseInt(!bash "-c" "echo $RANDOM");

# Generate a random integer between 'min' (inclusive) and 'max' (inclusive)
let randomInt(min, max) = {
    let maxBound = max - min + 1;
    min + parseInt(!bash "-c" ("echo $(( $RANDOM % " ~ toString(maxBound) ~ " ))"))
};


