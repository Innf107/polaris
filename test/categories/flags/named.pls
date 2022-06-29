# EXPECT: 1 2 3 ["3"] true
# ARGS:-f|1|2|-g|3|-h
options {
    "-f" (x, y)
    "-g" (g1) as g2
    "-h" as h
}

print(x, y, g1, g2, h)
