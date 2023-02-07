let f : forall a. a -> a = \x -> x

let g : a = let loop() = loop() in loop()
