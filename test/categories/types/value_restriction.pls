
let unsafeCoerce : forall a b. a -> b
let unsafeCoerce(x) = {
    let reference : forall a. Ref(List(a)) = ref []

    reference := [x]

    match reference! {
        [y] -> y
    }
}

print(unsafeCoerce("aaa") + 1)
