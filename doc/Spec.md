# Abstract Syntax

```
(Expression)
e ::= v                 (value)
    | e(e*)             (application)
    | { e; e }          (sequencing)
    | /x e*             (program call object)
    | (e1 | e2)         (pipe)
    | let x = e in e    (definition)
    | x := e            (assignment)

(Value) 
v ::= x                 (variable)
    | \(x+) -> e        (abstraction)

(Evaluation cxt)
E ::= <> 
    | E e 
    | v E 
    | { E; e } 
    | /x E* 
    | (E | e) 
    | (v | E)

(Forced evaluation cxt)
F ::= <>
    | v E
    | { E; e }
    | /x E*

(Side effects)
Ψ ::= run /x v*, Ψ
    | ∅
```

## Syntactic Sugar
```
{ e₁; ..; eₙ } = { e₁; { ..; eₙ } }
def f(x₁, .., xₙ){ e₁, .., eₙ } = (let f = \x₁, .., xₙ -> { e₁, .., eₙ})
e | x₂ v₂* = e | /x₂ v₂*
```

# Semantics
```
(Evaluation)
e₁ --> e₂, Ψ

(\(x₁,.., xₙ) -> e)(v₁,..,vₙ)  --> e[x₁:=v₁,..,xₙ:=vₙ], ∅ (app)

    e₁ |-->* v, Ψ
---------------------  (sequence)
{ e₁; e2 } |-->* e2, Ψ


/x v*, ∅ ~~> out, Ψ
----------------------------- (command force)
F[/x v*] --> F[reify(out)], Ψ


e, ∅ ~~> out₁, Ψ    /x₂ v₂*, out₁ ~~> out₂, Ψ₂
-------------------------------------   (pipe command)
F[e | /x₂ v₂*] --> F[reify(out₂)], Ψ₁ ∘ Ψ₂

(Command Evaluation)
/x v*, in ~~> vᵪ, out, run /x v* in (Internal, runs command)

e, in ~~> out₁, Ψ    /x₂ v₂*, out₁ ~~> out₂, Ψ₂
-----------------------------------------------   (pipe trans)
(e | /x₂ v₂*), in ~~> out₂, Ψ₁ ∘ Ψ₂

(Reification)
reify(out) ~~> v (Internal, awaits output stream)
```

