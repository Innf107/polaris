
let loop : forall a. a
let loop() = loop()

data Const(a, b) = a

let x : Const(a) = loop()
