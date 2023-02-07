# EXPECT: [1, 4, 4, 5, 6, 21, 22, 32, 534]

module List = import("../../../lib/list.pls")

print(List.sort([534,22,4,32,21,1,5,4,6]))
