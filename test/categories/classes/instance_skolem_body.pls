
class C(a) {
    m : forall b. b -> a
}

instance C(Int) {
    m(b) = {
        let _ : b = b 
        5
    }
}

