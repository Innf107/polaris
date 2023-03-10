# EXPECT: ABAB

let _ = async {
    sync {
        !printf "A"
        !sleep 0.2
        !printf "B"
    }
}
let _ = async {
    !sleep 0.1
    !printf "C"
    !sleep 0.2
    !printf "D"
}
