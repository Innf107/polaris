# EXPECT: 5

class C(a) {
    m : forall b. b -> a
}

instance C(Number) {
    m(b) = {
        let _ : b = b 
        5
    }
}

print(m("a"))
