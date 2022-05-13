
exception TODO
exception Panic of string

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

