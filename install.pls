#!/usr/bin/env polaris

options {
    "--bin-install-location" (installLocation = "/usr/bin/polaris")
    "--no-sudo" as noSudo
}

let List = require("lib/list.pls")

if installLocation == "" then {
    print("No installation to a custom location")
} else {
    let originalPath = $OPAM_SWITCH_PREFIX ~ "/bin/polaris";
    print("Copying file from '" ~ originalPath ~ "' to '" ~ installLocation ~ "'.");
    if noSudo then
        !cp originalPath installLocation
    else
        !sudo "cp" originalPath installLocation
}

print("Copying stdlib files to ~/.polaris/lib/")
!mkdir "-p" ($HOME ~ "/.polaris")
!cp "-r" "lib" ($HOME ~ "/.polaris")

print("Installation successful!")
