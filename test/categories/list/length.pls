# EXPECT: 5

module List = import("@std/list.pls")

print(List.length([1,2,3,4,5]))
