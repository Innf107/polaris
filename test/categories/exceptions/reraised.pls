
exception Panic(message : String) = "PANIC! " ~ message

let raisesInitially() = {
    raise (Panic("AAAAA"))
}

let reraises() = {
    try raisesInitially() {
        Panic(_) as exn -> raise exn
    }
}

try reraises() {
    Panic(_) as exn -> raise exn
}

 