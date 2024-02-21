# EXPECT: stat 'a.txt' 2> /dev/null

# TODO: The fix here is to implement interpolation through a type class
# so that we can properly generalize this to
# `doesFileExist : forall a. Interpolatable(a) => a -> ()`
# but until we have that, this is working "correctly"

let doesFileExist(file) = 
    try {
        # This needs to use !bash since polaris doesn't have a way to silence stderr yet
        print("stat '${file}' 2> /dev/null")
    } with {
        CommandFailure(_) -> ()
    }

doesFileExist("a.txt")