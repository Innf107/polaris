use crate::ast::*;

use std::collections::HashMap;

pub fn rename(expr: &Expr<String>) -> (Expr<FastName>, HashMap<FastName, String>) {
    let mut state = RenameState::new();

    let expr = rename_expr(expr, &mut state);

    (expr, panic!());
}

struct RenameState {
    encode_map: HashMap<String, FastName>,
    decode_map: HashMap<FastName, String>,

    name_counter: u64,
}

impl RenameState {
    fn new() -> RenameState {
        panic!()
    }

    fn new_name(&mut self) -> FastName {
        self.name_counter += 1;

        FastName::new(self.name_counter)
    }

    fn encode_or_new(&mut self, string: &str) -> FastName {
        match self.encode_map.get(string) {
            Some(fast_name) => *fast_name,
            None => {
                let fast_name = self.new_name();
                self.encode_map.insert(String::from(string), fast_name);
                self.decode_map.insert(fast_name, String::from(string));
                fast_name
            }
        }
    }
}

fn rename_expr(expr: &Expr<String>, rename_state: &mut RenameState) -> Expr<FastName> {
    match &expr.kind {
        ExprKind::Var(x) => {
            // let new_x = rename_state.encode_or_new(x);
            // expr.update(ExprKind::Var(new_x))
            panic!()
        }
        ExprKind::App(f, args) => {
            let new_f = rename_expr(f, rename_state);
            let new_args = args.iter().map(|x|{ rename_expr(x, rename_state)}).collect();

            expr.update(ExprKind::App(Box::new(new_f), new_args))
        }
        ExprKind::Sequence(exprs) => {
            let new_exprs = exprs.iter().map(|x|{ rename_expr(x, rename_state)}).collect();
            expr.update(ExprKind::Sequence(new_exprs))
        }
        ExprKind::CallProg(prog_name, args) => {
            let new_args = args.iter().map(|x|{ rename_expr(x, rename_state)}).collect();

            expr.update(ExprKind::CallProg(prog_name.to_string(), new_args))
        }
        ExprKind::Pipe(prog1, prog2) => {
            let new_prog1 = rename_expr(prog1, rename_state);
            let new_prog2 = rename_expr(prog2, rename_state);
            
            expr.update(ExprKind::Pipe(Box::new(new_prog1), Box::new(new_prog2)))
        }
        ExprKind::Let(_x, _expr) => {
            panic!()
        }
        ExprKind::Assign(_x, _expr) => {
            panic!()
        }
    }
}
