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
                write!(f, "(lambda ({}) {})", args.join(" "), body)
            },
            Atom::Var(var) => write!(f, "{}", var),
            Atom::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
            Atom::Int(x, _) => write!(f, "{}", x),
            Atom::Float(x, _) => write!(f, "{}", f64::from_bits(*x)),
            Atom::String(s) => write!(f, "\"{}\"", s),
            Atom::Char(c) => write!(f, "\"{}\"", c),
            Atom::Unit => write!(f, "'()"),
            Atom::Tuple(fields) => write!(f, "(list {})", fmap(fields, ToString::to_string).join(" ")),
            Atom::Extern(name, typ) => {
                match name.as_str() {
                    "putchar" => write!(f, "(lambda (x k) (k (display x)))"),
                    "malloc" => write!(f, "(lambda (x k) (k (list x)))"),
                    _ => write!(f, "(extern {name} : {typ})"),
                }
            }
            Atom::Effect(id, typ) => write!(f, "(effect {id} : {typ})"),
            Atom::MemberAccess(lhs, index) => write!(f, "(nth {} {})", lhs, index),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::If(c, t, e) => {
                write!(f, "(if {} {} {})", c, t, e)
            },
            Expr::Assign(var, rhs, expr) => {
                write!(f, "(block (set! {} {}) {})", var, rhs, expr)
            },
            Expr::LetRec(var, rhs, expr) => {
                write!(f, "(letrec ([{} {}]) {})", var, rhs, expr)
            },
            Expr::Call(function, args) =>{
                let args = fmap(args, ToString::to_string);
                write!(f, "({} {})", function, args.join(" "))
            },
            Expr::Builtin(builtin, k) => {
                write!(f, "({} {})", k, builtin)
            },
            Expr::Atom(atom) => atom.fmt(f),
            Expr::Handle(handle) => {
                write!(f, "[handle {} | {} {} -> {}]", handle.expr, handle.effect_fn, handle.k, handle.handler)
            },
            Expr::Switch(tag, cases, else_case) => {
                write!(f, "(case {}", tag)?;

                for (i, case) in cases.iter() {
                    write!(f, " [{} {}]", i, case)?;
                }

                if let Some(case) = else_case {
                    write!(f, " [else {}]", case)?;
                }

                write!(f, ")")
            },
        }
    }
}

impl std::fmt::Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Builtin::AddInt(lhs, rhs) => write!(f, "(+ {} {})", lhs, rhs),
            Builtin::AddFloat(lhs, rhs) => write!(f, "(+ {} {})", lhs, rhs),
            Builtin::SubInt(lhs, rhs) => write!(f, "(- {} {})", lhs, rhs),
            Builtin::SubFloat(lhs, rhs) => write!(f, "(- {} {})", lhs, rhs),
            Builtin::MulInt(lhs, rhs) => write!(f, "(* {} {})", lhs, rhs),
            Builtin::MulFloat(lhs, rhs) => write!(f, "(* {} {})", lhs, rhs),
            Builtin::DivSigned(lhs, rhs) => write!(f, "(/ {} {})", lhs, rhs),
            Builtin::DivUnsigned(lhs, rhs) => write!(f, "(/ {} {})", lhs, rhs),
            Builtin::DivFloat(lhs, rhs) => write!(f, "(/ {} {})", lhs, rhs),
            Builtin::ModSigned(lhs, rhs) => write!(f, "(% {} {})", lhs, rhs),
            Builtin::ModUnsigned(lhs, rhs) => write!(f, "(% {} {})", lhs, rhs),
            Builtin::ModFloat(lhs, rhs) => write!(f, "(% {} {})", lhs, rhs),
            Builtin::LessSigned(lhs, rhs) => write!(f, "(< {} {})", lhs, rhs),
            Builtin::LessUnsigned(lhs, rhs) => write!(f, "(< {} {})", lhs, rhs),
            Builtin::LessFloat(lhs, rhs) => write!(f, "(< {} {})", lhs, rhs),
            Builtin::EqInt(lhs, rhs) => write!(f, "(= {} {})", lhs, rhs),
            Builtin::EqFloat(lhs, rhs) => write!(f, "(= {} {})", lhs, rhs),
            Builtin::EqChar(lhs, rhs) => write!(f, "(= {} {})", lhs, rhs),
            Builtin::EqBool(lhs, rhs) => write!(f, "(= {} {})", lhs, rhs),

            // Leaving out some information in this block for easier compilation to racket
            Builtin::SignExtend(lhs, _) => write!(f, "{}", lhs),
            Builtin::ZeroExtend(lhs, _) => write!(f, "{}", lhs),
            Builtin::SignedToFloat(lhs, _) => write!(f, "{}", lhs),
            Builtin::UnsignedToFloat(lhs, _) => write!(f, "{}", lhs),
            Builtin::FloatToSigned(lhs, _) => write!(f, "{}", lhs),
            Builtin::FloatToUnsigned(lhs, _) => write!(f, "{}", lhs),
            Builtin::FloatPromote(lhs) => write!(f, "{}", lhs),
            Builtin::FloatDemote(lhs) => write!(f, "{}", lhs),
            // Builtin::SignExtend(lhs, rhs) => write!(f, "(sign-extend {} to {})", lhs, rhs),
            // Builtin::ZeroExtend(lhs, rhs) => write!(f, "(zero-extend {} to {})", lhs, rhs),
            // Builtin::SignedToFloat(lhs, rhs) => write!(f, "({} to {})", lhs, rhs),
            // Builtin::UnsignedToFloat(lhs, rhs) => write!(f, "({} to {})", lhs, rhs),
            // Builtin::FloatToSigned(lhs, rhs) => write!(f, "({} to {})", lhs, rhs),
            // Builtin::FloatToUnsigned(lhs, rhs) => write!(f, "({} to {})", lhs, rhs),
            // Builtin::FloatPromote(lhs) => write!(f, "(promote {})", lhs),
            // Builtin::FloatDemote(lhs) => write!(f, "(demote {})", lhs),
            Builtin::BitwiseAnd(lhs, rhs) => write!(f, "(bitwise-and {} {})", lhs, rhs),
            Builtin::BitwiseOr(lhs, rhs) => write!(f, "(bitwise-or {} {})", lhs, rhs),
            Builtin::BitwiseXor(lhs, rhs) => write!(f, "(bitwise-xor {} {})", lhs, rhs),
            Builtin::BitwiseNot(lhs) => write!(f, "(bitwise-not {})", lhs),
            // Builtin::Truncate(lhs, typ) => write!(f, "(truncate {} to {})", lhs, typ),
            Builtin::Truncate(lhs, _) => write!(f, "{}", lhs),
            Builtin::Deref(lhs, _) => write!(f, "(nth {} 0)", lhs),
            Builtin::Offset(lhs, rhs, _) => write!(f, "(tail {} {})", lhs, rhs),
            // Builtin::Transmute(lhs, typ) => write!(f, "(transmute {} to {})", lhs, typ),
            Builtin::Transmute(lhs, _) => write!(f, "{}", lhs),
            Builtin::StackAlloc(lhs) => write!(f, "(list {})", lhs),
        }
    }
}
