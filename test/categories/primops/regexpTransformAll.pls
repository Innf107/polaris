# EXPECT: test <b>aaa</b> test2 <b>bbb</b>

print(regexpTransformAll("<code>(.+?)</code>", \groups -> "<b>" ~ groups[1] ~ "</b>", "test <code>aaa</code> test2 <code>bbb</code>"))
