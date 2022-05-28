# EXPECT:5

let promises = [async x | x <- [1 .. 100]];

print([await p | p <- promises]);


