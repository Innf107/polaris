#!/usr/bin/env polaris
options {
    "-s" "--sync" as sync: "Execute tests synchronously instead of in parallel"
    "-e" "--exclude" (*) as exclude: "Exclude category from tests"
    "--use-dune" as useDune: "Run tests using 'dune exec polaris' instead of 'polaris'"
}

module List = import("../lib/list.pls")

# Silently ignore failures
let silent(cont) = try cont() with {
    # I *really* need to add record patterns
    CommandFailure(result) -> result.stdout
}

let for = if sync then List.for else List.forConcurrent

let doesFileExist(file) = 
    try {
        # This needs to use !bash since polaris doesn't have a way to silence stderr yet
        let _ = !bash "-c" ("stat '" ~ file ~ "' 2> /dev/null")
        true 
    } with {
        CommandFailure(_) -> false
    }
let stripExtension(path) = (!dirname path) ~ "/" ~ (!basename "-s" ".pls" path)

let categories = lines(!find (scriptLocal("categories")) "-mindepth" 1 "-maxdepth" 1 "-not" "-name" "*.disabled" [["-not", "-name", ex] | let ex <- exclude])

if categories == [] then {
    print("No tests left to run.");
    exit(0);
} else {

}

let files = lines(!find categories "-name" "*.pls")

let errors = ref 0

if useDune then {
    let _ = !dune "build"
}
else {}

for(files, \file -> {
    let expectation = silent (\ -> !grep "-Po" "(?<=# EXPECT: ).+" file);
    let args = split("|", silent(\ -> !grep "-Po" "(?<=# ARGS: ).+" file));

    let result = 
        if useDune then 
            # dune produces .exe files, even on linux
            silent (\ -> !_build/default/bin/main.exe file args)
        else
            silent (\ -> !polaris file args)


    if doesFileExist (stripExtension(file) ~ ".error") then {
        # This is an 'error' test, meaning the program is meant to fail
        # with the error message contained in the '.error' file

        let expectedError = !cat (stripExtension(file) ~ ".error")

        if result == expectedError then {
            !echo "-e" ("\e[32m[" ~ file ~ "](error): PASSED\e[0m")
            ()
        } else {
            !echo "-e" ("\e[1m\e[31m[" ~ file ~ "](error): FAILED!\n\e[0m"
                    ~ "\e[31m[    EXPECTED: '" ~ expectedError ~ "'\n"
                    ~ "      ACTUAL: '" ~ result ~ "'\e[0m")
            errors := errors! + 1
        }
    } else {
        if result == expectation then {
            # This is a regular 'run' test, so the program should succeed
            # and return the string in 'expectation'
        
            # This has to use echo, since polaris does not support string escapes 
            # at the moment.
            !echo "-e" ("\e[32m[" ~ file ~ "]: PASSED\e[0m")
            ()
        } else {
            !echo "-e" ("\e[1m\e[31m[" ~ file ~ "]: FAILED!\n\e[0m"
                    ~ "\e[31m    EXPECTED: '" ~ expectation ~ "'\n"
                    ~ "      ACTUAL: '" ~ result ~ "'\e[0m")
            errors := errors! + 1
        }
    };
})

if errors! == 0 then {
    !echo "-e" "\e[32mAll test passed.\e[0m"
    ()
} else {
    !echo "-e" ("\e[31m" ~ toString(errors!) ~ " TESTS FAILED!\e[0m")
    exit(errors!)
}
