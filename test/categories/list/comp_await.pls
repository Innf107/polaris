# EXPECT:[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

let promises = [async x | x <- [1 .. 10]];

print([await p | p <- promises]);


