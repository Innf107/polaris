# EXPECT: (5, true)

let startTime = parseInt(!date "+%s");

let x = try {
    let _ = async !sleep 1
    5
} with {}

let endTime = parseInt(!date "+%s");
print((x, endTime - startTime >= 1))

