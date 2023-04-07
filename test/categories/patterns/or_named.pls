# EXPECT: b

match [["a", "b"]] {
    [[_, x]] | [[_, "a"], [_, x]] -> {
        print(x)
    }
    _ -> ()
}
