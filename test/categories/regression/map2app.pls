# EXPECT: 

module List = import("../../../lib/list.pls")

let fst([x, _]) = x

let snd([_, x]) = x

let escapeHTML(content) = {
    let toReplace = 
        [ ["<", "&lt;"]
        , [">", "&gt;"]
        , ["<!--.*?-->", ""]
        ]

    List.foldr(\(x, r) -> regexpReplace(fst(x), snd(x), r), content, toReplace)
}
