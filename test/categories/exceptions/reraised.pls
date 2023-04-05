
exception Panic(message : String) = "PANIC! " ~ message

let raisesInitially() = {
    raise (Panic("AAAAA"))
}

let reraises() = {
    try raisesInitially() with {
        Panic(_) as exn -> raise exn
    }
}

try reraises() with {
    Panic(_) as exn -> raise exn
}

 