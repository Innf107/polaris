# EXPECT: ABAB

let _ = async {
    sync {
        !printf "A"
        !sleep 0.1
        !printf "B"
    }
}
let _ = async {
    sync {
        !printf "A"
        !sleep 0.1
        !printf "B"
    }
}
