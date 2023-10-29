use std::rc::Rc;

use crate::{hir::{IntegerKind, Type}, lexer::token::FloatKind};

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
    Tuple(Vec<Atom>),
    Extern(String, Type),
    Effect(Var, Type),
    MemberAccess(Box<Atom>, u32),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    If(Atom, Box<Expr>, Box<Expr>),
    Assign(Var, Atom, Box<Expr>),
    LetRec(Var, Atom, Box<Expr>),
    Builtin(Builtin, Box<Expr>),
    Call(Atom, Vec<Atom>),
    Switch(Atom, Vec<(u32, Expr)>, Option<Box<Expr>>),
    Handle(Handle),
}

#[derive(Debug, Clone)]
pub struct Handle {
    pub effect_fn: Var,
    pub k: Var,
    pub handler: Atom, // Expected to always be a Atom::Lambda
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Builtin {
    AddInt(Atom, Atom),
    AddFloat(Atom, Atom),

    SubInt(Atom, Atom),
    SubFloat(Atom, Atom),

    MulInt(Atom, Atom),
    MulFloat(Atom, Atom),

    DivSigned(Atom, Atom),
    DivUnsigned(Atom, Atom),
    DivFloat(Atom, Atom),

    ModSigned(Atom, Atom),
    ModUnsigned(Atom, Atom),
    ModFloat(Atom, Atom),

    LessSigned(Atom, Atom),
    LessUnsigned(Atom, Atom),
    LessFloat(Atom, Atom),

    EqInt(Atom, Atom),
    EqFloat(Atom, Atom),
    EqChar(Atom, Atom),
    EqBool(Atom, Atom),

    SignExtend(Atom, Type),
    ZeroExtend(Atom, Type),

    SignedToFloat(Atom, Type),
    UnsignedToFloat(Atom, Type),
    FloatToSigned(Atom, Type),
    FloatToUnsigned(Atom, Type),
    FloatPromote(Atom),
    FloatDemote(Atom),

    BitwiseAnd(Atom, Atom),
    BitwiseOr(Atom, Atom),
    BitwiseXor(Atom, Atom),
    BitwiseNot(Atom),

    Truncate(Atom, Type),
    Deref(Atom, Type),
    Offset(Atom, Atom, Type), // The Type is the element type
    Transmute(Atom, Type),

    /// Allocate space for the given value on the stack, and store it there. Return the stack address
    StackAlloc(Atom),
}
