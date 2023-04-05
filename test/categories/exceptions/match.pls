# EXPECT: A(1) | B(2) | Something else

exception A(x : Number) = "AAAA"

exception B(x : Number) = "B"

exception C() = "C"


let catchAB(cont) = 
    try cont() {
        A(x) -> "A(" ~ toString(x) ~ ")"
        exn -> 
            match exn {
                B(x) -> "B(" ~ toString(x) ~ ")"
                _ -> "Something else"
            }
    }

print(catchAB(\ -> raise A(1)) ~ " | " ~ catchAB(\ -> "aaa" ~ raise B(2)) ~ " | " ~ catchAB(\ -> raise C()))
