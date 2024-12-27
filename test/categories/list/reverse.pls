# EXPECT: [5, 4, 3, 2, 1]

module List = import("@std/list.pls")

print(List.reverse([1..5]))
