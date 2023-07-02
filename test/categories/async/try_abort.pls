# EXPECT: Aborted

exception A() = ""

try {
    let _ = async {
        !sleep 0.5
        raise A()
    }
    !sleep 1
    print("Not aborted!")
} with {
    A -> print("Aborted")
}