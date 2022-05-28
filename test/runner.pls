#!/usr/bin/env polaris
let List = require("list.pls");

let files = lines(!find (scriptLocal("categories")) "-name" "*.pls");

let errors = [];

List.for(files, \file -> {
    let expectation = !grep "-Po" "(?<=# EXPECT:).+" file;

    let result = !polaris file;

    if result == expectation then {
        # This has to use echo, since polaris does not support string escapes 
        # at the moment.
        !echo "-e" ("\e[32m[" ~ file ~ "]: PASSED\e[0m");
        ()
    } else {
        print("b");
        !echo "-e" ("\e[31m[" ~ file ~ "]: FAILED!");
        !echo "-e" ("    EXPECTED: '" ~ expectation ~ "'");
        !echo "-e" ("      ACTUAL: '" ~ result ~ "'");
        ()
    };
});
