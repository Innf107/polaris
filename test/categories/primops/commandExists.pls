# EXPECT: true true false true

print(
    commandExists("polaris"),
    commandExists("ls"),
    commandExists("definitelynonexistant"),
    commandExists("/usr/bin/ls")
    )
