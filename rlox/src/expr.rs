#![allow(dead_code)]

use crate::scanner::Token;
use pastey::paste;

macro_rules! exprs {
    ($($ty:ident { $($name:ident: $attr:ty),*})+) => {
        pub enum Expr {
            $($ty($ty)),+
        }

        impl Expr {
            pub fn accept<R>(&self, visitor: &impl ExprVisitor<R>) -> R {
                paste! {
                match self {
                    $(Expr::$ty(e) => visitor.[<visit_ $ty:lower _expr>](e)),+
                }
                }
            }
            
            pub fn accept_mut<R>(&self, visitor: &mut impl ExprVisitorMut<R>) -> R {
                paste! {
                match self {
                    $(Expr::$ty(e) => visitor.[<visit_ $ty:lower _expr>](e)),+
                }
                }
            }

            paste! {
                $(pub fn [< $ty:snake>]($($name: $attr),*) -> Box<Self> {
                    Box::new(Self::$ty($ty {
                        $($name,)*
                    }))
                })+
            }
        }

        $(pub struct $ty {
            $(pub $name: $attr),*
        })+

        paste! {
            pub trait ExprVisitor<R> {
                $(fn [<visit_ $ty:lower _expr>](&self, expr: &$ty) -> R;)+
            }
            pub trait ExprVisitorMut<R> {
                $(fn [<visit_ $ty:lower _expr>](&mut self, expr: &$ty) -> R;)+
            }

            impl<T: ExprVisitor<R>, R> ExprVisitorMut<R> for T {
                $(fn [<visit_ $ty:lower _expr>](&mut self, expr: &$ty) -> R {
                    ExprVisitor::[<visit_ $ty:lower _expr>](self, expr)
                })+
            }
        }
    }
}


exprs!{
    Assign {name: Token, value: Box<Expr>}
    Binary {left: Box<Expr>, op: Token, right: Box<Expr>}
    Call {callee: Box<Expr>, paren: Token, args: Vec<Box<Expr>>}
    Get {object: Box<Expr>, name: Token}
    Set {object: Box<Expr>, name: Token, value: Box<Expr>}
    Grouping {expression: Box<Expr>}
    Literal {value: crate::scanner::Literal}
    Logical {left: Box<Expr>, op: Token, right: Box<Expr>}
    Sup {name: Token, method: Token}
    This {name: Token}
    Unary {op: Token, right: Box<Expr>}
    Variable {name: Token}
}

