# EXPECT: ["1", "2", "3"]
# ARGS:-f|1|--flag|2|-f|3

options {
    "-f" "--flag" (*) as flags
}

print(flags)
