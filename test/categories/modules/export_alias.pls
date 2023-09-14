# EXPECT: [1]

module Test = import("./export_alias_base.pls.skip")

let x : Test.Test(Number) = [1]

print(x)
