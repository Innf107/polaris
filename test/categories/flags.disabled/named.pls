# EXPECT: 1 2 3 true a b c
# ARGS: -f|1|2|-g|3|-h
options {
    "-f" (x, y)
    "-g" (g1)
    "-h" as h
    "--default" (d1 = "a", d2 = "b", d3 = "c")
}

print(x, y, g1, h, d1, d2, d3)
