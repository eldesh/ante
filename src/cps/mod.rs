//! This CPS conversion algorithm is adapted from https://matt.might.net/articles/cps-conversion/
use std::{collections::{HashMap, HashSet, VecDeque}, rc::Rc};

use crate::hir::{DefinitionId, Type};
use crate::util::fmap;

pub mod ir;
mod printer;

use crate::hir::DefinitionInfo;
use ir::*;

use crate::hir::{ self, Ast };

pub fn convert_to_cps(hir: &hir::Ast) -> Expr {
    let mut context = Context::default();
    let mut cps = context.tk(hir, Box::new(|_, _| Expr::Atom(Atom::Unit)));

    while let Some((id, hir)) = context.queue.pop() {
        if !context.visited.insert(id) {
            continue;
        }

        let var = context.id_map[&id].clone();
        match hir.as_ref() {
            Ast::Definition(definition) => {
                cps = context.tk(&definition.expr, Box::new(move |_, expr| {
                    Expr::LetRec(var, expr, Box::new(cps))
                }));
            },
            other => unreachable!("Unexpected definition kind: {:?}", other),
        }
    }

    cps
}

#[derive(Default)]
struct Context {
    next_symbol: u64,
    id_map: HashMap<DefinitionId, Var>,
    visited: HashSet<DefinitionId>,
    queue: Vec<(DefinitionId, Rc<hir::Ast>)>,
}

impl Context {
    fn tk<'a>(&mut self, expr: &hir::Ast, k: Box<dyn FnOnce(&mut Self, Atom) -> Expr + 'a>) -> Expr {
        match expr {
            // atoms
            Ast::Literal(literal) => {
                let atom = self.convert_literal(literal);
                k(self, atom)
            }
            Ast::Variable(variable) => {
                let atom = self.convert_variable(variable);
                k(self, atom)
            }
            Ast::Lambda(lambda) => {
                let atom = self.convert_lambda(lambda);
                k(self, atom)
            }
            Ast::Extern(extern_expr) => {
                let atom = Atom::Extern(extern_expr.name.clone(), extern_expr.typ.clone());
                k(self, atom)
            },
            Ast::Effect(effect) => {
                let effect = self.convert_effect(effect);
                k(self, Atom::Var(effect))
            },

            // expressions
            Ast::Sequence(seq) => {
                if seq.statements.is_empty() {
                    k(self, Atom::Unit)
                } else if seq.statements.len() == 1 {
                    self.tk(&seq.statements[0], k)
                } else {
                    let first = &seq.statements[0];
                    let rest = &seq.statements[1..];
                    let rest = Ast::Sequence(hir::Sequence { statements: rest.to_vec() });
                    self.tk(first, Box::new(move |this, _| this.tk(&rest, k)))
                }
            }

            Ast::If(if_) => {
                // We have to reify the cont to avoid
                // a possible code blow-up:
                let rv = self.gensym("rv");
                self.tk(&if_.condition, Box::new(move |this, aexp| {
                    let body = k(this, Atom::Var(rv.clone()));
                    let cont = Atom::Lambda(vec![rv.clone()], Box::new(body));
                    let t = this.tc(&if_.then, cont.clone());
                    let e = this.tc(&if_.otherwise, cont);
                    Expr::If(aexp, Box::new(t), Box::new(e))
                }))
            }

            Ast::Assignment(assign) => {
                self.tk(&assign.lhs, Box::new(move |this, lhs| {
                    this.tk(&assign.rhs, Box::new(move |this, rhs| {
                        let rest = k(this, Atom::Unit);
                        Expr::Assign(lhs.into_var(), rhs, Box::new(rest))
                    }))
                }))
            }
   
            Ast::Definition(definition) => {
                let var = self.define(definition.variable, &definition.name);
                self.tk(&definition.expr, Box::new(move |this, expr| {
                    let rest = k(this, Atom::Unit);
                    Expr::LetRec(var, expr, Box::new(rest))
                }))
            }
            Ast::FunctionCall(..) => {
                let rv = self.gensym("rv");
                let body = k(self, Atom::Var(rv.clone()));
                self.tc(expr, Atom::Lambda(vec![rv], Box::new(body)))
            }
            Ast::Builtin(builtin) => self.tk_builtin(builtin, k),
            Ast::Tuple(tuple) => {
                self.tks(&tuple.fields, Box::new(move |this, exprs| {
                    k(this, Atom::Tuple(exprs.into()))
                }))
            }
            Ast::MemberAccess(member_access) => {
                self.tk(&member_access.lhs, Box::new(move |this, lhs| {
                    let atom = Atom::MemberAccess(Box::new(lhs), member_access.member_index);
                    k(this, atom)
                }))
            },
            Ast::Match(match_expr) => {
                let rv = self.gensym("rv");
                let body = k(self, Atom::Var(rv.clone()));
                let cont = Atom::Lambda(vec![rv.clone()], Box::new(body));
                self.handle_decision_tree(&match_expr.decision_tree, &match_expr.branches, cont)
            }
            Ast::Return(_return_expr) => todo!(),
            Ast::ReinterpretCast(cast) => {
                self.tk_builtin(&hir::Builtin::Transmute(cast.lhs.clone(), cast.target_type.clone()), k)
            },
            Ast::Handle(handle) => self.convert_handle(handle, k),
        }
    }

    fn convert_handle<'a>(&mut self, handle: &hir::Handle, k: Box<dyn FnOnce(&mut Self, Atom) -> Expr + 'a>) -> Expr {
        let expr = self.tk(&handle.expression, k);
        let effect_fn = self.convert_effect(&handle.effect);

        let k_var = self.define(handle.resume.definition_id, &handle.resume.name);
        let handler = self.convert_lambda(&handle.branch_body);

        Expr::Handle(Handle {
            effect_fn,
            k: k_var,
            handler,
            expr: Box::new(expr),
        })
    }

    fn tc(&mut self, expr: &hir::Ast, c: Atom) -> Expr {
        match expr {
            // atoms
            Ast::Literal(literal) => {
                let atom = self.convert_literal(literal);
                Expr::Call(c, vec![atom])
            }
            Ast::Variable(variable) => {
                let atom = self.convert_variable(variable);
                Expr::Call(c, vec![atom])
            }
            Ast::Lambda(lambda) => {
                let atom = self.convert_lambda(lambda);
                Expr::Call(c, vec![atom])
            }
            Ast::Extern(extern_expr) => {
                let atom = Atom::Extern(extern_expr.name.clone(), extern_expr.typ.clone());
                Expr::Call(c, vec![atom])
            },
            Ast::Effect(effect) => {
                let atom = self.convert_effect(effect);
                Expr::Call(c, vec![Atom::Var(atom)])
            },
    
            // expressions
            Ast::Sequence(seq) => {
                if seq.statements.is_empty() {
                    Expr::Call(c, vec![Atom::Unit])
                } else if seq.statements.len() == 1 {
                    self.tc(&seq.statements[0], c)
                } else {
                    let first = &seq.statements[0];
                    let rest = &seq.statements[1..];
                    let rest = Ast::Sequence(hir::Sequence { statements: rest.to_vec() });
                    // TODO: Verify this is correct
                    self.tk(first, Box::new(move |this, _| this.tc(&rest, c)))
                }
            }
            
            Ast::If(if_) => {
                let exprc = &if_.condition;
                let exprt = &if_.then;
                let exprf = &if_.otherwise;

                // We have to bind the cont to avoid
                // a possible code blow-up:
                let k = self.gensym("k");
                let t = self.tc(exprt, Atom::Var(k.clone()));
                let e = self.tc(exprf, Atom::Var(k.clone()));
                let body = self.tk(exprc, Box::new(move |_, aexp| Expr::If(aexp, Box::new(t), Box::new(e))));
                let lambda = Atom::Lambda(vec![k], Box::new(body));
                Expr::Call(lambda, vec![c])
            }

            Ast::Assignment(assign) => {
                self.tk(&assign.lhs, Box::new(move |this, lhs| {
                    this.tk(&assign.rhs, Box::new(move |_, rhs| {
                        Expr::Assign(lhs.into_var(), rhs, Box::new(Expr::Call(c, vec![Atom::Unit])))
                    }))
                }))
            }
       
            Ast::Definition(definition) => {
                let lhs = self.define(definition.variable, &definition.name);
                self.tk(&definition.expr, Box::new(move |_, rhs| {
                    Expr::LetRec(lhs, rhs, Box::new(Expr::Call(c, vec![Atom::Unit])))
                }))
            }

            Ast::FunctionCall(call) => {
                self.tk(&call.function, Box::new(move |this, f| {
                    this.tks(&call.args, Box::new(move |_, mut es| {
                        es.push_back(c);
                        Expr::Call(f, es.into())
                    }))
                }))
            }
            Ast::Builtin(builtin) => self.tc_builtin(builtin, c),
            Ast::MemberAccess(member_access) => {
                self.tk(&member_access.lhs, Box::new(move |_, lhs| {
                    let atom = Atom::MemberAccess(Box::new(lhs), member_access.member_index);
                    Expr::Call(c, vec![atom])
                }))
            },
            Ast::Tuple(tuple) => {
                self.tks(&tuple.fields, Box::new(move |_, exprs| {
                    let atom = Atom::Tuple(exprs.into());
                    Expr::Call(c, vec![atom])
                }))
            },
            Ast::Handle(handle) => {
                self.convert_handle(handle, Box::new(|_, expr| Expr::Call(c, vec![expr])))
            },
            Ast::Match(match_expr) => {
                let k = self.gensym("k");
                let cont = Atom::Var(k.clone());
                let tree = self.handle_decision_tree(&match_expr.decision_tree, &match_expr.branches, cont);
                let lambda = Atom::Lambda(vec![k], Box::new(tree));
                Expr::Call(lambda, vec![c])
            }
            Ast::Return(_return_expr) => todo!(),
            Ast::ReinterpretCast(cast) => {
                self.tc_builtin(&hir::Builtin::Transmute(cast.lhs.clone(), cast.target_type.clone()), c)
            },
        }
    }

    fn tks<'a>(&mut self, exprs: &[hir::Ast], k: Box<dyn FnOnce(&mut Self, VecDeque<Atom>) -> Expr + 'a>) -> Expr {
        if exprs.is_empty() {
            k(self, VecDeque::new())
        } else {
            let first = &exprs[0];
            let rest = &exprs[1..];
            self.tk(first, Box::new(move |this, hd| {
                this.tks(rest, Box::new(move |this, mut tl| {
                    tl.push_front(hd);
                    k(this, tl)
                }))
            }))
        }
    }

    fn convert_lambda(&mut self, lambda: &hir::Lambda) -> Atom {
        let k = self.gensym("k");
        let mut vars = fmap(&lambda.args, |info| {
            self.define(info.definition_id, &info.name)
        });
        vars.push(k.clone());

        let body = self.tc(&lambda.body, Atom::Var(k));
        Atom::Lambda(vars, Box::new(body))
    }

    fn convert_effect(&mut self, effect: &hir::Effect) -> Var {
        let id = effect.id;
        if let Some(var) = self.id_map.get(&id) {
            return var.clone();
        }

        self.define(id, &None)
    }

    fn convert_variable(&mut self, variable: &hir::Variable) -> Atom {
        Atom::Var(self.lookup(&variable))
    }

    fn convert_literal(&mut self, literal: &hir::Literal) -> Atom {
        match literal {
            hir::Literal::Bool(value) => Atom::Bool(*value),
            hir::Literal::Unit => Atom::Unit,
            hir::Literal::CString(str) => Atom::String(str.to_owned()),
            hir::Literal::Integer(value, kind) => Atom::Int(*value, *kind),
            hir::Literal::Float(value, kind) => Atom::Float(*value, *kind),
            hir::Literal::Char(value) => Atom::Char(*value),
        }
    }

    fn gensym(&mut self, name: &str) -> Var {
        self.next_symbol += 1;
        Var(self.next_symbol, Rc::new(name.to_owned()))
    }

    fn define(&mut self, id: DefinitionId, name: &Option<String>) -> Var {
        let new_var = self.gensym(name.as_ref().map(|s| s.as_str()).unwrap_or("v"));
        self.id_map.insert(id, new_var.clone());
        new_var
    }

    fn lookup(&mut self, info: &DefinitionInfo) -> Var {
        let id = info.definition_id;
        if let Some(var) = self.id_map.get(&id) {
            return var.clone();
        }

        let name = &info.name;
        let definition = match info.definition.clone() {
            Some(v) => v,
            None => unreachable!("Variable {:?} not found in lookup", &info.name.as_ref()),
        };
        self.queue.push((id, definition));
        self.define(id, name)
    }

    fn tk_builtin<'a>(&mut self, builtin: &hir::Builtin, k: Box<dyn FnOnce(&mut Self, Atom) -> Expr + 'a>) -> Expr {
        let rv = self.gensym("rv");
        let rest = k(self, Atom::Var(rv));
        self.convert_builtin(builtin, rest)
    }

    fn tc_builtin(&mut self, builtin: &hir::Builtin, c: Atom) -> Expr {
        self.convert_builtin(builtin, Expr::Atom(c))
    }

    fn convert_builtin(&mut self, builtin: &hir::Builtin, rest: Expr) -> Expr {
        let rest = Box::new(rest);

        let one_arg = |ctor: fn(Atom) -> Builtin, this: &mut Self, lhs, rest| {
            this.tk(lhs, Box::new(move |_, lhs| {
                Expr::Builtin(ctor(lhs), rest)
            }))
        };

        let one_arg_and_type = |ctor: fn(Atom, Type) -> Builtin, this: &mut Self, lhs, typ: &Type, rest| {
            this.tk(lhs, Box::new(move |_, lhs| {
                Expr::Builtin(ctor(lhs, typ.clone()), rest)
            }))
        };

        let two_args = |ctor: fn(Atom, Atom) -> Builtin, this: &mut Self, lhs, rhs, rest| {
            this.tk(lhs, Box::new(move |this, lhs| {
                this.tk(rhs, Box::new(move |_, rhs| {
                    Expr::Builtin(ctor(lhs, rhs), rest)
                }))
            }))
        };

        match builtin {
            hir::Builtin::AddInt(l, r) => two_args(Builtin::AddInt, self, l, r, rest),
            hir::Builtin::AddFloat(l, r) => two_args(Builtin::AddFloat, self, l, r, rest),
            hir::Builtin::SubInt(l, r) => two_args(Builtin::SubInt, self, l, r, rest),
            hir::Builtin::SubFloat(l, r) => two_args(Builtin::SubFloat, self, l, r, rest),
            hir::Builtin::MulInt(l, r) => two_args(Builtin::MulInt, self, l, r, rest),
            hir::Builtin::MulFloat(l, r) => two_args(Builtin::MulFloat, self, l, r, rest),
            hir::Builtin::DivSigned(l, r) => two_args(Builtin::DivSigned, self, l, r, rest),
            hir::Builtin::DivUnsigned(l, r) => two_args(Builtin::DivUnsigned, self, l, r, rest),
            hir::Builtin::DivFloat(l, r) => two_args(Builtin::DivFloat, self, l, r, rest),
            hir::Builtin::ModSigned(l, r) => two_args(Builtin::ModSigned, self, l, r, rest),
            hir::Builtin::ModUnsigned(l, r) => two_args(Builtin::ModUnsigned, self, l, r, rest),
            hir::Builtin::ModFloat(l, r) => two_args(Builtin::ModFloat, self, l, r, rest),
            hir::Builtin::LessSigned(l, r) => two_args(Builtin::LessSigned, self, l, r, rest),
            hir::Builtin::LessUnsigned(l, r) => two_args(Builtin::LessUnsigned, self, l, r, rest),
            hir::Builtin::LessFloat(l, r) => two_args(Builtin::LessFloat, self, l, r, rest),
            hir::Builtin::EqInt(l, r) => two_args(Builtin::EqInt, self, l, r, rest),
            hir::Builtin::EqFloat(l, r) => two_args(Builtin::EqFloat, self, l, r, rest),
            hir::Builtin::EqChar(l, r) => two_args(Builtin::EqChar, self, l, r, rest),
            hir::Builtin::EqBool(l, r) => two_args(Builtin::EqBool, self, l, r, rest),
            hir::Builtin::SignExtend(l, r) => one_arg_and_type(Builtin::SignExtend, self, l, r, rest),
            hir::Builtin::ZeroExtend(l, r) => one_arg_and_type(Builtin::ZeroExtend, self, l, r, rest),
            hir::Builtin::SignedToFloat(l, r) => one_arg_and_type(Builtin::SignedToFloat, self, l, r, rest),
            hir::Builtin::UnsignedToFloat(l, r) => one_arg_and_type(Builtin::UnsignedToFloat, self, l, r, rest),
            hir::Builtin::FloatToSigned(l, r) => one_arg_and_type(Builtin::FloatToSigned, self, l, r, rest),
            hir::Builtin::FloatToUnsigned(l, r) => one_arg_and_type(Builtin::FloatToUnsigned, self, l, r, rest),
            hir::Builtin::FloatPromote(l) => one_arg(Builtin::FloatPromote, self, l, rest),
            hir::Builtin::FloatDemote(l) => one_arg(Builtin::FloatDemote, self, l, rest),
            hir::Builtin::BitwiseAnd(l, r) => two_args(Builtin::BitwiseAnd, self, l, r, rest),
            hir::Builtin::BitwiseOr(l, r) => two_args(Builtin::BitwiseOr, self, l, r, rest),
            hir::Builtin::BitwiseXor(l, r) => two_args(Builtin::BitwiseXor, self, l, r, rest),
            hir::Builtin::BitwiseNot(l) => one_arg(Builtin::BitwiseNot, self, l, rest),
            hir::Builtin::Truncate(l, r) => one_arg_and_type(Builtin::Truncate, self, l, r, rest),
            hir::Builtin::Deref(l, r) => one_arg_and_type(Builtin::Deref, self, l, r, rest),
            hir::Builtin::Transmute(l, r) => one_arg_and_type(Builtin::Transmute, self, l, r, rest),
            hir::Builtin::StackAlloc(l) => one_arg(Builtin::StackAlloc, self, l, rest),
            hir::Builtin::Offset(l, r, size) => {
                self.tk(l, Box::new(move |this, lhs| {
                    this.tk(r, Box::new(move |_, rhs| {
                        Expr::Builtin(Builtin::Offset(lhs, rhs, *size), rest)
                    }))
                }))
            },
        }
    }

    // TODO: Bind branches to variables to avoid branch repetition
    fn handle_decision_tree(&mut self, decision_tree: &hir::DecisionTree, branches: &Vec<hir::Ast>, c: Atom) -> Expr {
        match decision_tree {
            hir::DecisionTree::Leaf(index) => self.tc(&branches[*index], c),
            hir::DecisionTree::Definition(definition, rest) => {
                let lhs = self.define(definition.variable, &definition.name);

                self.tk(&definition.expr, Box::new(move |this, rhs| {
                    let rest = this.handle_decision_tree(rest, branches, c);
                    Expr::LetRec(lhs, rhs, Box::new(rest))
                }))
            },
            hir::DecisionTree::Switch { int_to_switch_on, cases, else_case } => {
                self.tk(&int_to_switch_on, Box::new(move |this, int_to_switch_on| {
                    let cases = fmap(cases, |(index, case)| {
                        (*index, this.handle_decision_tree(case, branches, c.clone()))
                    });

                    let else_case = else_case.as_ref().map(|case| {
                        Box::new(this.handle_decision_tree(case, branches, c))
                    });

                    Expr::Switch(int_to_switch_on, cases, else_case)
                }))
            },
        }
    }
}

impl Atom {
    fn into_var(self) -> Var {
        match self {
            Atom::Var(var) => var,
            other => unreachable!("Atom::into_var called on {:?}", other),
        }
    }
}
