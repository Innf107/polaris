
type 'name expr = Var of 'name                              (* x *)
                | App of 'name expr * 'name expr list       (* e (e₁, .., eₙ) *)
                | Lambda of 'name list * 'name expr         (* \(x₁, .., xₙ) -> e*)
                | Seq of ('name expr) list                  (* { e₁ ; .. ; eₙ } *)
                | LetSeq of 'name * 'name expr              (* let x = e (Only valid inside `Seq` expressions) *)

                | Let of 'name * 'name expr * 'name expr    (* let x = e1 in e2 (valid everywhere) *)
                | Assign of 'name * 'name expr              (* x := e *)

                | ProgCall of string * 'name expr list      (* /p e₁ .. eₙ *)
                | Pipe of 'name expr list                   (* (e₁ | .. | eₙ) *)

type name = { name: string; index: int }



