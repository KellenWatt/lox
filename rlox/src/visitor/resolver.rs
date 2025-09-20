use crate::expr::{self, Expr, ExprVisitorMut};
use crate::stmt::{self, Stmt, StmtVisitorMut};
use crate::scanner::{Token};
use std::collections::{HashMap, VecDeque};
use crate::visitor::interpreter::Interpreter;
use crate::error;

#[derive(Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Clone, Copy)]
enum ClassType {
    None,
    Class,
    Subclass,
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: VecDeque<HashMap<String, bool>>,
    current_func: FunctionType,
    current_class: ClassType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Resolver<'a> {
        Resolver {
            interpreter,
            scopes: VecDeque::new(),
            current_func: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve_stmt_list(&mut self, stmts: &[Stmt]) {
        for stmt in stmts.iter() {
            self.resolve_stmt(stmt);
        }
    }
    

    pub fn resolve_stmt(&mut self, stmt: &Stmt) {
        stmt.accept_mut(self);
    }
    

    pub fn resolve_expr(&mut self, expr: &Expr) {
        expr.accept_mut(self);
    }
    

    fn begin_scope(&mut self) {
        self.scopes.push_back(HashMap::new())
    }
    

    fn end_scope(&mut self) {
        self.scopes.pop_back();
    }

    fn declare(&mut self, name: &Token) {
        let Some(scope) = self.scopes.back_mut() else {
            return;
        };
        if scope.contains_key(&name.lexeme) {
            error::token_error(&name, "Aready a variable with this name in this scope");
        }
        scope.insert(name.lexeme.clone(), false);
    }

    fn define(&mut self, name: &Token) {
        let Some(scope) = self.scopes.back_mut() else {
            return;
        };
        scope.insert(name.lexeme.clone(), true);
    }

    fn resolve_function(&mut self, func: &stmt::Function, kind: FunctionType) {
        let enclosing = self.current_func;
        self.current_func = kind;
        self.begin_scope();
        for param in func.params.iter() {
            self.declare(param);
            self.define(param);
        }
        self.resolve_stmt_list(&func.body);
        self.end_scope();
        self.current_func = enclosing;
    }

}

macro_rules! resolve_local {
    ($self:expr, $exp:expr, $name:expr) => {
        if $self.scopes.len() > 0 {
            for i in (0..$self.scopes.len()).rev() {
                if $self.scopes[i].contains_key(&$name.lexeme) {
                    // This relies on the given Expr having a name member. Return renames "keyword"
                    // to "name" to comply.
                    $self.interpreter.resolve(&$exp.name, $self.scopes.len() - 1 - i);
                    return;
                }
            }
        }
    }
}

impl<'a> StmtVisitorMut<()> for Resolver<'a> {
    fn visit_class_stmt(&mut self, stmt: &stmt::Class) {
        let enclosing = self.current_class;
        self.current_class = ClassType::Class;
        self.declare(&stmt.name);
        self.define(&stmt.name);
       
        if let Some(ref superclass) = stmt.superclass {
            if stmt.name.lexeme == superclass.name.lexeme {
                error::token_error(&superclass.name, "A class can't inherit from itself");
            }
            self.current_class = ClassType::Subclass;

            // This is a little hacky, but it prevents allowing more general expressions for the 
            // superclass in class definitions
            self.visit_variable_expr(superclass);

            self.begin_scope();
            self.scopes.back_mut().unwrap().insert("super".into(), true);
        }

        self.begin_scope();
        self.scopes.back_mut().unwrap().insert("this".into(), true);

        for method in &stmt.methods {
            let decl = if method.name.lexeme == "init" {
                FunctionType::Initializer
            } else {
                FunctionType::Method
            };
            self.resolve_function(method, decl);
        }
        self.end_scope();
        if stmt.superclass.is_some() {
            self.end_scope();
        }
        self.current_class = enclosing;
    }

    fn visit_var_stmt(&mut self, stmt: &stmt::Var) {
        self.declare(&stmt.name);
        if let Some(ref init) = stmt.init {
            self.resolve_expr(init);
        }
        self.define(&stmt.name);
    }

    fn visit_block_stmt(&mut self, stmt: &stmt::Block) {
        self.begin_scope();
        self.resolve_stmt_list(&stmt.statements);
        self.end_scope();
    }

    fn visit_function_stmt(&mut self, stmt: &stmt::Function) {
        self.declare(&stmt.name);
        self.define(&stmt.name);

        self.resolve_function(stmt, FunctionType::Function);
    }

    fn visit_expression_stmt(&mut self, stmt: &stmt::Expression) {
        self.resolve_expr(&stmt.expression);
    }

    fn visit_if_stmt(&mut self, stmt: &stmt::If) {
        self.resolve_expr(&stmt.condition);
        self.resolve_stmt(&stmt.then_branch);
        if let Some(ref els) = stmt.else_branch {
            self.resolve_stmt(els);
        }
    }

    fn visit_print_stmt(&mut self, stmt: &stmt::Print) {
        self.resolve_expr(&stmt.expression);
    }

    fn visit_return_stmt(&mut self, stmt: &stmt::Return) {
        if let FunctionType::None = self.current_func {
            error::token_error(&stmt.keyword, "Can't return from top-level code.");
        }
        if let Some(ref value) = stmt.value {
            if let FunctionType::Initializer = self.current_func {
                error::token_error(&stmt.keyword, "Can't return a value from an initializer");
            }
            self.resolve_expr(value);
        }
    }

    fn visit_while_stmt(&mut self, stmt: &stmt::While) {
        self.resolve_expr(&stmt.condition);
        self.resolve_stmt(&stmt.body);
    }
}

impl<'a> ExprVisitorMut<()> for Resolver<'a> {
    fn visit_variable_expr(&mut self, expr: &expr::Variable) {
        if let Some(scope) = self.scopes.back() {
            if scope.get(&expr.name.lexeme) == Some(&false) { // This is a weird way to phrase it.
                error::token_error(&expr.name, "Can't read local variable in its own initializer");
            }
        }
        resolve_local!(self, expr, &expr.name);
    }

    fn visit_assign_expr(&mut self, expr: &expr::Assign) {
        self.resolve_expr(&expr.value);
        resolve_local!(self, expr, &expr.name);
    }

    fn visit_binary_expr(&mut self, expr: &expr::Binary) {
        self.resolve_expr(&expr.left);
        self.resolve_expr(&expr.right);
    }

    fn visit_call_expr(&mut self, expr: &expr::Call) {
        self.resolve_expr(&expr.callee);
        for arg in expr.args.iter() {
            self.resolve_expr(arg);
        }
    }

    fn visit_get_expr(&mut self, expr: &expr::Get) {
        self.resolve_expr(&expr.object);
    }
    
    fn visit_set_expr(&mut self, expr: &expr::Set) {
        self.resolve_expr(&expr.value);
        self.resolve_expr(&expr.object);
    }


    fn visit_grouping_expr(&mut self, expr: &expr::Grouping) {
        self.resolve_expr(&expr.expression);
    }

    fn visit_literal_expr(&mut self, _expr: &expr::Literal) {}

    fn visit_logical_expr(&mut self, expr: &expr::Logical) {
        self.resolve_expr(&expr.left);
        self.resolve_expr(&expr.right);
    }

    fn visit_sup_expr(&mut self, expr: &expr::Sup) {
        if let ClassType::None = self.current_class {
            error::token_error(&expr.name, "Can't use 'super' outside of a class");
        } else if let ClassType::Class = self.current_class {
            error::token_error(&expr.name, "Can't use 'super' in a class with no superclass");
        }
        resolve_local!(self, expr, expr.name);
    }

    fn visit_this_expr(&mut self, expr: &expr::This) {
        if let ClassType::None = self.current_class {
            error::token_error(&expr.name, "Can't use 'this' outside of a class");
            return;
        }
        resolve_local!(self, expr, &expr.name);
    }

    fn visit_unary_expr(&mut self, expr: &expr::Unary) {
        self.resolve_expr(&expr.right);
    }
}
