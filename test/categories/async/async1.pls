# EXPECT: 5

let x = ref 1;

let update = async {
    !sleep "0.2";
    x := 5
};

await update;

print(x!);
