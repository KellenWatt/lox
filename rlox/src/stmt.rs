#![allow(dead_code)]
use pastey::paste;
use crate::expr::{self, Expr};
use crate::scanner::Token;
use std::rc::Rc;

macro_rules! stmts {
    ($($ty:ident { $($name:ident: $attr:ty),*})+) => {
        pub enum Stmt {
            $($ty($ty)),+
        }

        impl Stmt {
            pub fn accept<R>(&self, visitor: &impl StmtVisitor<R>) -> R {
                paste! {
                match self {
                    $(Stmt::$ty(e) => visitor.[<visit_ $ty:lower _stmt>](e)),+
                }
                }
            }
            
            pub fn accept_mut<R>(&self, visitor: &mut impl StmtVisitorMut<R>) -> R {
                paste! {
                match self {
                    $(Stmt::$ty(e) => visitor.[<visit_ $ty:lower _stmt>](e)),+
                }
                }
            }

            paste! {
                $(pub fn [< # $ty:snake>]($($name: $attr),*) -> Self {
                    Self::$ty($ty {
                        $($name,)*
                    })
                })+
            }
        }

        $(pub struct $ty {
            $(pub $name: $attr),*
        })+

        paste! {
            pub trait StmtVisitor<R> {
                $(fn [<visit_ $ty:lower _stmt>](&self, stmt: &$ty) -> R;)+
            }
            pub trait StmtVisitorMut<R> {
                $(fn [<visit_ $ty:lower _stmt>](&mut self, stmt: &$ty) -> R;)+
            }

            impl<T: StmtVisitor<R>, R> StmtVisitorMut<R> for T {
                $(fn [<visit_ $ty:lower _stmt>](&mut self, stmt: &$ty) -> R {
                    StmtVisitor::[<visit_ $ty:lower _stmt>](self, stmt)
                })+
            }
        }
    }
}

stmts! {
    Block {statements: Vec<Stmt>}
    Class {name: Token, superclass: Option<expr::Variable>, methods: Vec<Function>}
    Expression {expression: Box<Expr>}
    Print {expression: Box<Expr>}
    Var {name: Token, init: Option<Box<Expr>>}
    If {condition: Box<Expr>, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>>}
    While {condition: Box<Expr>, body: Box<Stmt>}
    Function {name: Token, params: Vec<Token>, body: Rc<Vec<Stmt>>}
    Return {keyword: Token, value: Option<Box<Expr>>}
}


impl Clone for Function {
    fn clone(&self) -> Function {
        Function {
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
        }
    }
}
