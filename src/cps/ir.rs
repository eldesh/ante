use std::rc::Rc;

use crate::{hir::IntegerKind, lexer::token::FloatKind};

/// A variable is represented by its unique id
/// and an optional name string for debugging
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var(pub(super) u64, pub(super) Rc<String>);

#[derive(Debug, Clone)]
pub enum Atom {
    Lambda(Vec<Var>, Box<Expr>),
    Var(Var),
    Bool(bool),
    Int(u64, IntegerKind),
    Float(u64, FloatKind),
    String(String),
    Char(char),
    Unit,
}

#[derive(Debug, Clone)]
pub enum Expr {
    If(Atom, Box<Expr>, Box<Expr>),
    Assign(Var, Atom, Box<Expr>),
    LetRec(Var, Atom, Box<Expr>),
    Call(Atom, Vec<Atom>),
}
