# EXPECT:[true, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]]

let Async = require("async.pls");

let startTime = parseInt(!date "+%s");

let promise = Async.all([(async {!sleep 1; x}) | x <- [1 .. 30]]);

let intermediateTime = parseInt(!date "+%s");

let result = await promise;

let endTime = parseInt(!date "+%s");


# Async.all should not wait for any promises to complete, but should return a new promise
# that waits for them.
print([intermediateTime - startTime < 1 && endTime - startTime >= 1 && endTime - startTime < 3, result]);
