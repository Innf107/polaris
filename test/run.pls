#!/usr/bin/env polaris
options {
    "-s" "--sync" as sync: "Execute tests synchronously instead of in parallel"
    "-e" "--exclude" (*) as exclude: "Exclude category from tests"
    "--use-polaris" as usePolaris: "Run tests using 'polaris' instead of 'dune exec -- polaris'"
    "--timeout" (timeout = "10s"): "Timeout to apply to the tests run. Defaults to 10s"
}

module List = import("../lib/list.pls")

# Silently ignore failures
let silent(cont) = try cont() with {
    # I *really* need to add record patterns
    CommandFailure(result) -> result.stdout
}

let succeeds(cont) = try { let _ = cont(); true } with {
    CommandFailure(_) -> false
}

let for = if sync then List.for else List.forConcurrent

let doesFileExist(file) = 
    try {
        # This needs to use !bash since polaris doesn't have a way to silence stderr yet
        let _ = !bash "-c" ("stat '${file}' 2> /dev/null")
        true 
    } with {
        CommandFailure(_) -> false
    }
let stripExtension(path) = "${!dirname path}/${!basename "-s" ".pls" path}"

let categories = lines(!find (scriptLocal("categories")) "-mindepth" 1 "-maxdepth" 1 "-not" "-name" "*.disabled" [["-not", "-name", ex] | let ex <- exclude])


if categories == [] then {
    print("No tests left to run.");
    exit(0);
} else {

}

let files = lines(!find categories "-name" "*.pls")

if not usePolaris then {
    let _ = !dune "build"
}
else {}

let errors = ref 0
let knownErrors = ref 0
let knownErrorsPassed = ref 0

let passed(file, isKnownFailure) = {
    if isKnownFailure then {
        print("\e[33m[${file}](error): PASSED but was marked as known failure\e[0m")
        knownErrorsPassed := knownErrorsPassed! + 1
    } else {
        print("\e[32m[${file}](error): PASSED\e[0m")
    }
}

let failed(file, isKnownFailure, expected, actual) = {
    if isKnownFailure then {
        print("\e[1m\e[35m[${file}](error): FAILED!\n\e[0m"
                ~ "\e[35m[    EXPECTED: '${expected}'\n"
                ~        "      ACTUAL: '${actual}'\e[0m")
        knownErrors := knownErrors! + 1
    } else {
        print("\e[1m\e[31m[${file}](error): FAILED!\n\e[0m"
                ~ "\e[31m[    EXPECTED: '${expected}'\n"
                ~        "      ACTUAL: '${actual}'\e[0m")
        errors := errors! + 1

    }
}


for(files, \file -> {
    let expectation = silent (\ -> !grep "-Po" "(?<=# EXPECT: ).+" file)
    let isKnownFailure = succeeds (\ -> !grep "-P" "# KNOWN" file)
    let args = split("|", silent(\ -> !grep "-Po" "(?<=# ARGS: ).+" file))

    let result = 
        if not usePolaris then 
            # dune produces .exe files, even on linux
            silent (\ -> !timeout timeout "_build/default/bin/main.exe" file args)
        else
            silent (\ -> !timeout timeout "polaris" file args)

    if doesFileExist("${stripExtension(file)}.error") then {
        # This is an 'error' test, meaning the program is meant to fail
        # with the error message contained in the '.error' file

        let expectedError = !cat "${stripExtension(file)}.error"

        if result == expectedError then {
            passed(file, isKnownFailure)
        } else {
            failed(file, isKnownFailure, expectedError, result)
        } 
    } else {
        if result == expectation then {
            # This is a regular 'run' test, so the program should succeed
            # and return the string in 'expectation'
            passed(file, isKnownFailure)
        } else {
            failed(file, isKnownFailure, expectation, result)
        }
    };
})

let disabled = {let x = !find "test/categories" "-type" "f" "-name" "*.disabled"; x} | wc "-l"
if errors! == 0 then {
    print("\e[32m\e[1mAll test passed. (${disabled} disabled, ${knownErrors!} known errors))\e[0m")
    if knownErrorsPassed! > 0 then {
        print("\e[1m\e[33m${knownErrorsPassed!} known errors have been fixed\e[0m")
    } else {}
    ()
} else {
    print("\e[31m${toString(errors!)} TESTS FAILED! (${disabled} disabled, ${knownErrors!} known errors)\e[0m")
    if knownErrorsPassed! > 0 then {
        print("\e[1m\e[33m${knownErrorsPassed!} known errors have been fixed\e[0m")
    } else {}
    exit(errors!)
}
