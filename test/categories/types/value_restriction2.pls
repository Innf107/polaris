let c = ref (\x -> x)

c := \x -> x + 1

let _ = c!(true)