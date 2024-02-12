type ('a, 'b) t =
  | This of 'a
  | That of 'b
  | Both of 'a * 'b
