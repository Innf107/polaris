# EXPECT: [("a", 0), ("b", 1), ("c", 2)]

module List = import("@std/list.pls")

print(List.indexed(["a", "b", "c"]))