#!/usr/bin/env polaris
options {
    "-s" "--sync" as sync: "Execute tests synchronously instead of in parallel"
    "-e" "--exclude" (*) as exclude: "Exclude category from tests"
    "--use-dune" as useDune: "Run tests using 'dune exec polaris' instead of 'polaris'"
}

let List = require("list.pls")

let for = if sync then List.for else List.forConcurrent

let doesFileExist(file) = (!bash "-c" ("stat '" ~ file ~ "' > /dev/null 2> /dev/null && echo 'true'")) == "true"
let stripExtension(path) = (!dirname path) ~ "/" ~ (!basename "-s" ".pls" path)

let categories = lines(!find (scriptLocal("categories")) "-mindepth" 1 "-maxdepth" 1 "-not" "-name" "*.disabled" [["-not", "-name", ex] | ex <- exclude])

if categories == [] then {
    print("No tests left to run.");
    exit(0);
} else {

}

let files = lines(!find categories "-name" "*.pls")

let errors = 0

# Build polaris synchronously first, since dune does not properly support concurrent builds.
# We have to use 'exec' with '--help', since dune cannot build executables
if useDune then
    !dune "exec" "--" "polaris" "--help"
else
    ()

for(files, \file -> {
    let expectation = !grep "-Po" "(?<=# EXPECT: ).+" file;
    let args = split("|", !grep "-Po" "(?<=# ARGS: ).+" file);

    let result = 
        if useDune then 
            !dune "exec" "polaris" "--" file args
        else
            !polaris file args
    

    if doesFileExist (stripExtension(file) ~ ".error") then {
        # This is an 'error' test, meaning the program is meant to fail
        # with the error message contained in the '.error' file

        let expectedError = !cat (stripExtension(file) ~ ".error")

        if result == expectedError then {
            !echo "-e" ("\e[32m[" ~ file ~ "](error): PASSED\e[0m")
        } else {
            !echo "-e" ("\e[31m[" ~ file ~ "]: FAILED!\n"
                    ~ "    EXPECTED: '" ~ expectedError ~ "'\n"
                    ~ "      ACTUAL: '" ~ result ~ "'\e[0m")
            errors := errors + 1
        }
    } else {
        if result == expectation then {
            # This is a regular 'run' test, so the program should succeed
            # and return the string in 'expectation'
        
            # This has to use echo, since polaris does not support string escapes 
            # at the moment.
            !echo "-e" ("\e[32m[" ~ file ~ "]: PASSED\e[0m")
        } else {
            !echo "-e" ("\e[31m[" ~ file ~ "]: FAILED!\n"
                    ~ "    EXPECTED: '" ~ expectation ~ "'\n"
                    ~ "      ACTUAL: '" ~ result ~ "'\e[0m")
            errors := errors + 1
        }
    };
})

if errors == 0 then {
    !echo "-e" "\e[32mAll test passed.\e[0m"
} else {
    !echo "-e" ("\e[31m" ~ errors ~ " TESTS FAILED!\e[0m")
    exit(errors)
}
