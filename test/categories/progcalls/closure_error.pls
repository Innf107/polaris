# EXPECT: 5

# We need to catch the error since the exception message will contain a random path like
# '/tmp/937030027/closure', which our test setup is currently not smart enough to deal with 
try {
    !env (\args -> 5) "a" "b"
    ()
} with {
    CommandFailure(failure) -> print(failure.exitCode)
}