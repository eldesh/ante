use crate::util::fmap;

use super::ir::*;

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.1, self.0)
    }
}

impl std::fmt::Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Lambda(args, body) => {
                let args = fmap(args, ToString::to_string);
                write!(f, "(fn {} -> {})", args.join(", "), body)
            },
            Atom::Var(var) => write!(f, "{}", var),
            Atom::Bool(b) => write!(f, "{}", b),
            Atom::Int(x, k) => write!(f, "{}{}", x, k),
            Atom::Float(x, k) => write!(f, "{}_{}", f64::from_bits(*x), k),
            Atom::String(s) => write!(f, "\"{}\"", s),
            Atom::Char(c) => write!(f, "'{}'", c),
            Atom::Unit => write!(f, "()"),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::If(c, t, e) => {
                write!(f, "(if {} then {} else {})", c, t, e)
            },
            Expr::Assign(var, rhs, expr) => {
                write!(f, "({} := {} in {})", var, rhs, expr)
            },
            Expr::LetRec(var, rhs, expr) => {
                write!(f, "({} = {} in {})", var, rhs, expr)
            },
            Expr::Call(function, args) =>{
                let args = fmap(args, ToString::to_string);
                write!(f, "({} {})", function, args.join(" "))
            },
        }
    }
}
