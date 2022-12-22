# EXPECT: [1, 2, 1, 2]

print([ x
    | let (3 | 5) <- [1..5]
    , let [x] <- [[1], [2]]
    ])
