# EXPECT: true

module List = import("../../../lib/list.pls");

let _ = List.forConcurrent + 0

let startTime = parseInt(!date "+%s");

List.forConcurrent([0 .. 100], \x -> {
    !sleep "1";
});

let endTime = parseInt(!date "+%s");

let secondsTaken = endTime - startTime;

# Check that the program waited for forConcurrent to execute
# but also that it did in fact evaluate everything concurrently
print(secondsTaken > 0 && secondsTaken < 3)
