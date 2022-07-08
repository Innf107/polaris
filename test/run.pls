#!/usr/bin/env polaris
options {
    "-s" "--sync" as sync: "Execute tests synchronously instead of in parralel"
    "-e" "--exclude" (*) as exclude: "Exclude category from tests"
    "--use-dune" as useDune: "Run test using 'dune exec polaris --' instead of 'polaris'"
}

let List = require("list.pls")

let for = if sync then List.for else List.forConcurrent


let categories = lines(!find (scriptLocal("categories")) "-mindepth" 1 "-maxdepth" 1 [["-not", "-name", ex] | ex <- exclude])

if categories == [] then {
    print("No tests left to run.");
    exit(0);
} else {

}

let files = lines(!find categories "-name" "*.pls")

let errors = 0

for(files, \file -> {
    let expectation = !grep "-Po" "(?<=# EXPECT: ).+" file;
    let args = split("|", !grep "-Po" "(?<=# ARGS: ).+" file);

    let result = 
        if useDune then 
            !dune "exec" "polaris" "--" file args
        else
            !polaris file args

    if result == expectation then {
        # This has to use echo, since polaris does not support string escapes 
        # at the moment.
        !echo "-e" ("\e[32m[" ~ file ~ "]: PASSED\e[0m")
    } else {
        !echo "-e" ("\e[31m[" ~ file ~ "]: FAILED!\n"
                  ~ "    EXPECTED: '" ~ expectation ~ "'\n"
                  ~ "      ACTUAL: '" ~ result ~ "'\e[0m")
        errors := errors + 1
    };
})

if errors == 0 then {
    !echo "-e" "\e[32mAll test passed.\e[0m"
} else {
    !echo "-e" "\e[31m" ~ errors ~ " TESTS FAILED!\e[0m"
}
