# EXPECT: 205

module List = import("@std/list.pls");

let x = ref 5;

let p = async {
    List.for([1 .. 100], \_ -> x := x! + 1)
};

List.for([1 .. 100], \_ -> x := x! + 1);

await p;

# No race condition
print(x!)
