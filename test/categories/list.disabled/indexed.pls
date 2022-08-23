# EXPECT: [["a", 0], ["b", 1], ["c", 2]]

let List = require("list.pls")

print(List.indexed(["a", "b", "c"]))