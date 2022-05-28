#!/usr/bin/env polaris
let List = require("list.pls");

let files = lines(!find (scriptLocal("categories")) "-name" "*.pls");

let errors = 0;

List.forConcurrent(files, \file -> {
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
