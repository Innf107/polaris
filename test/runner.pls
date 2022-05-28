#!/usr/bin/env polaris

let files = lines(!find (scriptLocal("categories")) "-name" "*.pls");

print(files);
