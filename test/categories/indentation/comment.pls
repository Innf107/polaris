# EXPECT:1
# Taken from the test runner

let files = []
let expectation = null
let result = null
let for = require("list.pls").for

for(files, \file -> {
    if result == expectation then {
        # This has to use echo, since polaris does not support string escapes 
        # at the moment.
        !echo "-e" ("\e[32m[" ~ file ~ "]: PASSED\e[0m");
        ()
    } else {
        ()
    };
})

print(1);
