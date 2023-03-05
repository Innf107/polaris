# EXPECT: Trying to access something at out of bounds index 5: Index out of bounds: 5

exception IndexOutOfBounds(index : Number) = "Index out of bounds: " ^ toString(index)


let f() = {
    raise(IndexOutOfBounds(5))
    print("EXCEPTION DID NOT ABORT THE PROGRAM!!!")
}

try f() with {
    IndexOutOfBounds(index) as exn -> print("Trying to access something at out of bounds index " ^ toString(index) ^ ": " ^ exceptionMessage(exn))
}
