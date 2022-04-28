open Script
open Script.Ast

let test = [
    LetSeq ("x", (Lambda (["x"], (Var "x"))))
  ; Var "x"
  ]

let run (exprs : string expr list) =
  let renamed = Rename.rename exprs in
  print_endline (Ast.pretty renamed)

let () = 
  run test

