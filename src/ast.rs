#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub struct FastName {
    pub name_id: u64,
}

impl FastName {
    pub fn new(id: u64) -> FastName {
        return FastName { name_id: id };
    }
}

pub struct Expr<Name> {
    pub kind: ExprKind<Name>,
}

impl<Name> Expr<Name>{
    pub fn update<OtherName>(&self, new_kind: ExprKind<OtherName>) -> Expr<OtherName> {
        Expr {
            kind: new_kind
        }
    }
}

pub enum ExprKind<Name> {
    Var(Name),
    App(Box<Expr<Name>>, Vec<Expr<Name>>),
    Sequence(Vec<Expr<Name>>),
    CallProg(String, Vec<Expr<Name>>),
    Pipe(Box<Expr<Name>>, Box<Expr<Name>>),
    Let(Name, Box<Expr<Name>>),
    Assign(Name, Box<Expr<Name>>),
}
