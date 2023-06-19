//! This CPS conversion algorithm is adapted from https://matt.might.net/articles/cps-conversion/
use std::{collections::{HashMap, HashSet}, rc::Rc};

use crate::hir::DefinitionId;
use crate::util::fmap;

pub mod ir;
mod printer;

use crate::hir::DefinitionInfo;
use ir::*;

use crate::hir::{ self, Ast };

pub fn convert_to_cps(hir: &hir::Ast) -> Expr {
    let mut context = Context::default();
    let mut cps = context.tc(hir, Atom::Unit);

    while let Some((id, hir)) = context.queue.pop() {
        if !context.visited.insert(id) {
            continue;
        }

        let var = context.id_map[&id].clone();
        println!("Creating letrec for {} {} from queue", var.0, var.1);

        match hir.as_ref() {
            Ast::Definition(definition) => {
                cps = context.tk(&definition.expr, &|_, expr| {
                    println!("rhs for {} {} is {}", var.0, var.1, expr);
                    Expr::LetRec(var.clone(), expr, Box::new(cps.clone()))
                });
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
    fn tk(&mut self, expr: &hir::Ast, k: &dyn Fn(&mut Self, Atom) -> Expr) -> Expr {
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
                    self.tk(first, &|this, _| this.tk(&rest, k))
                }
            }

            Ast::If(if_) => {
                let exprc = &if_.condition;
                let exprt = &if_.then;
                let exprf = &if_.otherwise;

                // We have to reify the cont to avoid
                // a possible code blow-up:
                let rv = self.gensym("rv");
                self.tk(exprc, &move |this, aexp| {
                    let body = k(this, Atom::Var(rv.clone()));
                    let cont = Atom::Lambda(vec![rv.clone()], Box::new(body));
                    let t = this.tc(exprt, cont.clone());
                    let e = this.tc(exprf, cont);
                    Expr::If(aexp, Box::new(t), Box::new(e))
                })
            }

            Ast::Assignment(assign) => {
                self.tk(&assign.lhs, &|this, lhs| {
                    this.tk(&assign.rhs, &|this, rhs| {
                        let rest = k(this, Atom::Unit);
                        Expr::Assign(lhs.clone().into_var(), rhs, Box::new(rest))
                    })
                })
            }
   
            Ast::Definition(definition) => {
                let var = self.define(definition.variable, &definition.name);
                self.tk(&definition.expr, &|this, expr| {
                    let rest = k(this, Atom::Unit);
                    Expr::LetRec(var.clone(), expr, Box::new(rest))
                })
            }

            Ast::FunctionCall(..) => {
                let rv = self.gensym("rv");
                let body = k(self, Atom::Var(rv.clone()));
                self.tc(expr, Atom::Lambda(vec![rv], Box::new(body)))
            }
            Ast::Match(_) => todo!(),
            Ast::Return(_) => todo!(),
            Ast::Extern(_) => todo!(),
            Ast::MemberAccess(_) => todo!(),
            Ast::Tuple(_) => todo!(),
            Ast::ReinterpretCast(_) => todo!(),
            Ast::Builtin(_) => todo!(),
        }
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
                    self.tk(first, &|this, _| this.tc(&rest, c.clone()))
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
                let body = self.tk(exprc, &|_, aexp| Expr::If(aexp, Box::new(t.clone()), Box::new(e.clone())));
                let lambda = Atom::Lambda(vec![k], Box::new(body));
                Expr::Call(lambda, vec![c])
            }

            Ast::Assignment(assign) => {
                self.tk(&assign.lhs, &|this, lhs| {
                    this.tk(&assign.rhs, &|_, rhs| {
                        Expr::Assign(lhs.clone().into_var(), rhs, Box::new(Expr::Call(c.clone(), vec![Atom::Unit])))
                    })
                })
            }
       
            Ast::Definition(definition) => {
                let lhs = self.define(definition.variable, &definition.name);
                // let rest = self.tc(e, c);
                // Expr::LetRec(vs, rhss, Box::new(rest))
                self.tk(&definition.expr, &|_, rhs| {
                    Expr::LetRec(lhs.clone(), rhs, Box::new(Expr::Call(c.clone(), vec![Atom::Unit])))
                })
            }
            
            Ast::FunctionCall(call) => {
                self.tk(&call.function, &|this, f| {
                    this.tks(&call.args, &|_, es| {
                        let mut es = es.to_vec();
                        es.push(c.clone());
                        Expr::Call(f.clone(), es)
                    })
                })
            }
            Ast::Match(_) => todo!(),
            Ast::Return(_) => todo!(),
            Ast::Extern(_) => todo!(),
            Ast::MemberAccess(_) => todo!(),
            Ast::Tuple(_) => todo!(),
            Ast::ReinterpretCast(_) => todo!(),
            Ast::Builtin(_) => todo!(),
        }
    }

    fn tks(&mut self, exprs: &[hir::Ast], k: &dyn Fn(&mut Self, &[Atom]) -> Expr) -> Expr {
        if exprs.is_empty() {
            k(self, &[])
        } else {
            let first = &exprs[0];
            let rest = &exprs[1..];
            self.tk(first, &|this, hd| {
                this.tks(rest, &|this, tl| {
                    let mut v = tl.to_vec();
                    v.insert(0, hd.clone());
                    k(this, &v)
                })
            })
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
            None => unreachable!("Variable {} not found in lookup", &info.name.as_ref().unwrap()),
        };
        self.queue.push((id, definition));
        self.define(id, name)
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
