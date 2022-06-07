# EXPECT:[[1, "a"], [2, "b"], [3, "c"]]

let zip(xs, ys) = match [xs, ys] {
    [[], _] -> []
    [_, []] -> []
    [x : xs, y : ys] -> [[x, y]] ~ zip(xs, ys)
}

print(zip([1,2,3], ["a", "b", "c"]))
