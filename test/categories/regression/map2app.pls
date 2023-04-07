# EXPECT: 

module List = import("../../../lib/list.pls")

let escapeHTML(content) = {
    let toReplace = 
        [ ["<", "&lt;"]
        , [">", "&gt;"]
        , ["<!--.*?-->", ""]
        ]

    List.foldr(\(x, r) -> regexpReplace(x, x, r), content, toReplace)
}
