# EXPECT: 5

module Test = import("export_types.pls.skip")

let r : Test.Test = Test.Test({ x = 5 })

print(Test.f(r))
