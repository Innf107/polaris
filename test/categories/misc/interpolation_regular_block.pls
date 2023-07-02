# EXPECT: stat 'a.txt' 2> /dev/null

let doesFileExist(file) = 
    try {
        # This needs to use !bash since polaris doesn't have a way to silence stderr yet
        print("stat '${file}' 2> /dev/null")
    } with {
        CommandFailure(_) -> ()
    }

doesFileExist("a.txt")