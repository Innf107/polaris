# EXPECT: AA

exception A() = "AA"

try raise A() {
    exn -> print(exceptionMessage(exn))
}
