# EXPECT: testaaaabbbbccccddddeeeeffff

let expectation = "bbbb"
let result = "eeee"

!echo "-e" ("test"
          ~ "aaaa" ~ expectation ~ "cccc"
          ~ "dddd" ~ result ~ "ffff")