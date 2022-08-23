# EXPECT: 205

let List = require("list.pls");

let x = 5;

let p = async {
    List.for([1 .. 100], \_ -> x := x + 1)
};

List.for([1 .. 100], \_ -> x := x + 1);

await p;

# No race condition
print(x)
