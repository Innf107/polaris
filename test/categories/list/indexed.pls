# EXPECT: [("a", 0), ("b", 1), ("c", 2)]

module List = import("../../../lib/list.pls")

print(List.indexed(["a", "b", "c"]))