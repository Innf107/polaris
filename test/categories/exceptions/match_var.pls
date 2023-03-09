# EXPECT: AA

exception A = "AA"

try raise A() with {
    exn -> print(exceptionMessage(exn))
}
