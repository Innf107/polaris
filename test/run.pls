#!/usr/bin/env polaris
options {
    "--sync" as sync: "Execute tests synchronously instead of in parralel"
}

let List = require("list.pls");

let for = if sync then List.for else List.forConcurrent;

let files = lines(!find (scriptLocal("categories")) "-name" "*.pls");

let errors = 0;

for(files, \file -> {
    let expectation = !grep "-Po" "(?<=# EXPECT:).+" file;

    let result = !polaris file;

    if result == expectation then {
        # This has to use echo, since polaris does not support string escapes 
        # at the moment.
        !echo "-e" ("\e[32m[" ~ file ~ "]: PASSED\e[0m");
        ()
    } else {
        !echo "-e" ("\e[31m[" ~ file ~ "]: FAILED!\n"
                  ~ "    EXPECTED: '" ~ expectation ~ "'\n"
                  ~ "      ACTUAL: '" ~ result ~ "'\e[0m");
        errors := errors + 1;
    };
});

if errors == 0 then {
    !echo "-e" "\e[32mAll test passed.\e[0m";
    ()
} else {
    !echo "-e" "\e[31m" ~ errors ~ " TESTS FAILED!\e[0m";
    ()
}