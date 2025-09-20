#![allow(dead_code)]

use crate::expr::{self, Expr, ExprVisitor};
use itertools::Itertools;


pub struct Printer;


impl Printer {
    pub fn print(&self, e: &Expr) -> String {
        e.accept(self)
    }

    fn parenthesize(&self, name: &str, args: &[&Expr]) -> String {
        format!("({} {})", name, args.into_iter().map(|exp| {
            exp.accept(self)
        }).join(" "))
    }
}

impl ExprVisitor<String> for Printer {
    fn visit_binary_expr(&self, e: &expr::Binary) -> String{
        self.parenthesize(&e.op.lexeme, &[&e.left, &e.right])
    }

    fn visit_grouping_expr(&self, e: &expr::Grouping) -> String{
        self.parenthesize("group", &[&e.expression])
    }
    
    fn visit_literal_expr(&self, e: &expr::Literal) -> String {
        e.value.to_string()
    }
    
    fn visit_unary_expr(&self, e: &expr::Unary) -> String {
        self.parenthesize(&e.op.lexeme, &[&e.right])
    }
}
