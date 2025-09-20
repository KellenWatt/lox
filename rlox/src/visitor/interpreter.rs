use crate::expr::{self, Expr, ExprVisitorMut};
use crate::stmt::{self, Stmt, StmtVisitorMut};
use crate::scanner::{self, Token, TokenType};
use crate::error::{runtime_error, RuntimeError};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::time::SystemTime;

#[derive(Debug, thiserror::Error)]
enum LoxError {
    #[error("{msg}")]
    Runtime{token: Token, msg: String},
    #[error("You should never be seing this")]
    Return{value: LoxValue},
}

#[derive(Clone)]
enum FunctionKind {
    Lox {
        name: String,
        params: Vec<Token>,
        body: Rc<Vec<Stmt>>,
        closure: Option<Box<Environment>>,
    },
    Native(Rc<dyn Fn(&[LoxValue]) -> Result<LoxValue, LoxError>>),
    Constructor {
        class: Rc<Class>,
        body: Option<Rc<Function>>
    },
}

#[derive(Clone)]
pub struct Function {
    arity: usize,
    body: FunctionKind,
    is_init: bool,
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.body {
            FunctionKind::Lox{name, ..} => {
                write!(f, "<fun {}>", name)
            }
            FunctionKind::Native(_) => {
                write!(f, "<native fun>")
            }
            FunctionKind::Constructor{class, ..} => {
                write!(f, "{} constructor", class.name)
            }
        }
    }
}


impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Function {
    fn call(&self, interp: &mut Interpreter, args: Vec<LoxValue>) -> Result<LoxValue, LoxError> {
        match &self.body {
            FunctionKind::Lox{params, body, closure, ..} => {
                let mut env = Environment::enclosing(closure.clone().unwrap_or_else(|| interp.globals.clone()));
                for (i, param) in params.iter().enumerate() {
                    env.define(&param.lexeme, args[i].clone());
                }
                let this = env.get_at(1, &Token{
                        ty: TokenType::This,
                        lexeme: "this".into(),
                        literal: None,
                        line: 0
                    }).unwrap();
                match interp.execute_block(body, env) {
                    Ok(_) => Ok(if self.is_init {this} else {LoxValue::Nil}),
                    Err(LoxError::Return{value}) => Ok(if self.is_init {this} else {value}),
                    Err(e) => Err(e)
                }
            }
            FunctionKind::Native(f) => {
                f(&args)
            }
            FunctionKind::Constructor{class, body} => {
                let object = Rc::new(Object::new(class.clone()));
                if let Some(cons) = body {
                    cons.bind(object.clone()).call(interp, args)?;
                }
                Ok(LoxValue::Object(object))
            }
        }
    }

    fn lox(func: &stmt::Function, closure: Option<Box<Environment>>) -> Function {
        Function {
            arity: func.params.len(),
            body: FunctionKind::Lox{
                name: func.name.lexeme.clone(),
                params: func.params.clone(),
                body: func.body.clone(),
                closure,
            },
            is_init: false,
        }
    }

    fn native<F: Fn(&[LoxValue]) -> Result<LoxValue, LoxError> + 'static>(f: F, arity: usize) -> Function {
        Function {
            arity,
            body: FunctionKind::Native(Rc::new(f)),
            is_init: false,
        }
    }

    fn bind(&self, this: Rc<Object>) -> Function {
        let FunctionKind::Lox{name, params, body, closure} = &self.body else {
            // This shouldn't be rust-level, but everything else is more complicated
            panic!("Can't bind a non-lox function"); 
        };
        let mut env = Environment::enclosing(closure.clone().unwrap());
        env.define("this".into(), LoxValue::Object(this));
        Function {
            arity: self.arity,
            body: FunctionKind::Lox {
                name: name.clone(),
                params: params.clone(),
                body: body.clone(),
                closure: Some(env),
            },
            is_init: self.is_init,
        }
    }
}

#[derive(Clone)]
pub struct Class {
    name: String,
    parent: Option<Rc<Class>>,
    constructor: Function,
    methods: HashMap<String, Function>,
}

impl std::fmt::Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "class {}", self.name)
    }
}

impl Class {
    fn find_method(&self, name: &str) -> Option<Function> {
        self.methods.get(name).cloned().or_else(|| {
            self.parent.as_ref().and_then(|parent| {
                parent.find_method(name)
            })
        })
    }
}

#[derive(Clone)]
pub struct Object {
    class: Rc<Class>,
    members: RefCell<HashMap<String, LoxValue>>,
}

impl Object {
    fn new(class: Rc<Class>) -> Object {
        Object {
            class,
            members: RefCell::new(HashMap::new()),
        }
    }

    fn get(this: Rc<Object>, name: &Token) -> Result<LoxValue, LoxError> {
        this.clone().members.borrow().get(&name.lexeme).cloned().or_else(|| {
            let method = this.class.find_method(&name.lexeme)?;
            Some(LoxValue::Func(method.bind(this)))
        }).ok_or_else(|| LoxError::Runtime {
            token: name.clone(),
            msg: format!("Undefined property '{}'", name.lexeme),
        })
    }

    fn set(&self, name: Token, value: LoxValue) {
        self.members.borrow_mut().insert(name.lexeme, value);
    }
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} instance", self.class.name)
    }
}


#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Number(f64),
    Str(String),
    Func(Function),
    Class(Class), 
    Object(Rc<Object>),
}


impl PartialEq for LoxValue {
    fn eq(&self, right: &LoxValue) -> bool {
        use LoxValue::*;
        match (self, right) {
            (Nil, Nil) => true,
            (Bool(a), Bool(b)) => a == b,
            (Number(a), Number(b)) => a == b,
            (Str(a), Str(b)) => a == b,
            _ => false,
        }
    }
}

impl std::fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use LoxValue::*;
        match self {
            Nil => write!(f, "nil"),
            Bool(b) => write!(f, "{}", b),
            Number(n) => write!(f, "{}", n),
            Str(s) => write!(f, "{}", s),
            Func(fun) => write!(f, "{}", fun),
            Class(c) => write!(f, "{:?}", c),
            Object(o) => write!(f, "{:?}", o),
        }
    }
}

impl LoxValue {
    fn from_literal(val: scanner::Literal) -> Option<LoxValue> {
        use scanner::Literal::*;
        match val {
            Nil => Some(LoxValue::Nil),
            Bool(v) => Some(LoxValue::Bool(v)),
            Number(n) => Some(LoxValue::Number(n)),
            String(s) => Some(LoxValue::Str(s)),
            Ident(_) => None
        }
    }

}



pub struct Interpreter {
    env: Option<Box<Environment>>,
    globals: Box<Environment>,
    locals: HashMap<Token, usize>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut env = Environment::new();
        env.define("clock", LoxValue::Func(Function::native(|_| {
            let now = SystemTime::now();
            let elapsed = now.duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs_f64();
            Ok(LoxValue::Number(elapsed))
        }, 0)));
        Interpreter {
            globals: env.clone(),
            env: Some(env),
            locals: HashMap::new(),
        }
    }
    
    pub fn interpret(&mut self, statements: Vec<Stmt>) {
        for statement in statements {
            if let Err(LoxError::Runtime{token, msg}) = self.execute(&statement) {
                runtime_error(RuntimeError::new(&token, &msg));
                break;
            }
        }
    }

    fn execute(&mut self, statement: &Stmt) -> Result<(), LoxError> {
        statement.accept_mut(self)
    }

    fn execute_block(&mut self, statements: &Vec<Stmt>, env: Box<Environment>) -> Result<(), LoxError> {
        let prev = self.env.take();
        self.env = Some(env);
        for stmt in statements {
            if let Err(e) = self.execute(stmt) {
                self.env = prev;
                return Err(e);
            }
        }
        self.env = prev;
        Ok(())
    }

    fn evaluate(&mut self, exp: &Expr) -> Result<LoxValue, LoxError> {
        exp.accept_mut(self)
    }

    fn is_truthy(obj: &LoxValue) -> bool {
        match obj {
            LoxValue::Nil | LoxValue::Bool(false) => false,
            _ => true,
        }
    }

    fn is_equal(a: &LoxValue, b: &LoxValue) -> bool {
        a == b
    }

    pub fn resolve(&mut self, token: &Token, depth: usize) {
        self.locals.insert(token.clone(), depth);
    }

    fn look_up_variable<T>(&mut self, name: &Token, _exp: T) -> Result<LoxValue, LoxError> {
        if let Some(distance) = self.locals.get(name) {
            self.env.as_ref().unwrap().get_at(*distance, &name)
        } else {
            self.globals.get(&name)
        }
    }
}

macro_rules! binary_number_op {
    ($left:expr, $op:tt, $right:expr, $output:expr, $else: expr) => {
        if let LoxValue::Number(l) = $left {
            if let LoxValue::Number(r) = $right {
                Ok($output(l $op r))
            } else {
                $else
            }
        } else {
            $else
        }
    }
}

impl StmtVisitorMut<Result<(), LoxError>> for Interpreter {
    fn visit_block_stmt(&mut self, stmt: &stmt::Block) -> Result<(), LoxError> {
        let env = Environment::enclosing(self.env.clone().unwrap());
        self.execute_block(&stmt.statements, env)?;
        Ok(())
    }

    fn visit_class_stmt(&mut self, stmt: &stmt::Class) -> Result<(), LoxError> {
    
        let mut superclass = None;
        if let Some(ref sup) = stmt.superclass {
            // weirdness for the same reason as the resolver. Prevents something like 1 + 2.
            let sup = self.visit_variable_expr(&sup)?;
            if let LoxValue::Class(cls) = sup {
                superclass = Some(cls);
            } else {
                return Err(LoxError::Runtime{token: stmt.superclass.as_ref().unwrap().name.clone(), msg: "Superclass must be a class".into()});
            }
        }

        self.env.as_mut().unwrap().define(&stmt.name.lexeme, LoxValue::Nil);

        if stmt.superclass.is_some() {
            self.env = Some(Environment::enclosing(self.env.clone().unwrap()));
            self.env.as_mut().unwrap().define("super", LoxValue::Class(superclass.clone().unwrap()));
        }

        let superclass = superclass.map(|s| Rc::new(s));

        let mut methods = HashMap::new();
        for method in stmt.methods.iter() {
            let func = Function {
                arity: method.params.len(),
                body: FunctionKind::Lox {
                    name: method.name.lexeme.clone(),
                    params: method.params.clone(),
                    body: method.body.clone(),
                    closure: self.env.clone(),
                },
                is_init: &method.name.lexeme == "init"
            };
            methods.insert(method.name.lexeme.clone(), func);
        }


        let constructor = methods.get("init").map(|f| {
            Rc::new(f.clone())
        });

        let mut class = Class {
            name: stmt.name.lexeme.clone(),
            parent: superclass,
            constructor: Function{
                arity: constructor.as_ref().map(|f| f.arity).unwrap_or(0),
                // This is a placeholder, since the constructor needs to refer to the class we're creating
                body: FunctionKind::Native(Rc::new(|_| {unreachable!()})),
                is_init: false,
            },
            methods,
        };
        let cons = FunctionKind::Constructor {
            class: Rc::new(class.clone()),
            body: constructor,
        };

        class.constructor.body = cons;
        let class = LoxValue::Class(class);
        if stmt.superclass.is_some() {
            self.env = Some(self.env.as_ref().unwrap().enclosing.as_ref().unwrap().clone());
        }

        self.env.as_mut().unwrap().assign(&stmt.name, class)?;
        Ok(())
    }
    
    fn visit_expression_stmt(&mut self, stmt: &stmt::Expression) -> Result<(), LoxError> {
        self.evaluate(&stmt.expression)?;
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &stmt::If) -> Result<(), LoxError> {
        if Self::is_truthy(&self.evaluate(&stmt.condition)?) {
            self.execute(&stmt.then_branch)?
        } else if let Some(ref els) = stmt.else_branch {
            self.execute(els)?;
        }
        Ok(())
    }

    fn visit_function_stmt(&mut self, stmt: &stmt::Function) -> Result<(), LoxError> {
        let fun = Function::lox(stmt, self.env.clone());
        self.env.as_mut().unwrap().define(&stmt.name.lexeme, LoxValue::Func(fun));
        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &stmt::Print) -> Result<(), LoxError> {
        let value = self.evaluate(&stmt.expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &stmt::Var) -> Result<(), LoxError> {
        let mut value = LoxValue::Nil;
        if let Some(ref init) = stmt.init {
            value = self.evaluate(init)?;
        }

        self.env.as_mut().unwrap().define(&stmt.name.lexeme, value);
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &stmt::While) -> Result<(), LoxError> {
        while Self::is_truthy(&self.evaluate(&stmt.condition)?) {
            self.execute(&stmt.body)?;
        }
        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &stmt::Return) -> Result<(), LoxError> {
        let mut res = LoxValue::Nil;
        if let Some(ref value) = stmt.value {
            res = self.evaluate(value)?;
        }
        Err(LoxError::Return{value: res})
    }
}

impl ExprVisitorMut<Result<LoxValue, LoxError>> for Interpreter {

    fn visit_literal_expr(&mut self, exp: &expr::Literal) -> Result<LoxValue, LoxError> {
        Ok(LoxValue::from_literal(exp.value.clone()).unwrap())
    }

    fn visit_binary_expr(&mut self, exp: &expr::Binary) -> Result<LoxValue, LoxError> {
        let left = self.evaluate(&exp.left)?;
        let right = self.evaluate(&exp.right)?;
        
        let err = || Err(LoxError::Runtime{token: exp.op.clone(), msg: "Operands must be numbers".into()});

        
        match exp.op.ty {
            TokenType::Minus => binary_number_op!(left, -, right, LoxValue::Number, err()),
            TokenType::Slash => binary_number_op!(left, /, right, LoxValue::Number, err()),
            TokenType::Star => binary_number_op!(left, *, right, LoxValue::Number, err()),
            TokenType::Plus => {
                if let LoxValue::Number(l) = left {
                    if let LoxValue::Number(r) = right {
                        Ok(LoxValue::Number(l + r))
                    } else {
                        err()
                    }
                } else if let LoxValue::Str(l) = left {
                    if let LoxValue::Str(r) = right {
                        Ok(LoxValue::Str(l + &r))
                    } else {
                        Err(LoxError::Runtime{token: exp.op.clone(), msg: "Operands must be strings".into()})
                        // Err(RuntimeError::new(&exp.op, "Operands must be strings"))
                    }
                } else {
                    Err(LoxError::Runtime{token: exp.op.clone(), msg: "Operands must be two numbers or two strings".into()})
                    // Err(RuntimeError::new(&exp.op, "Operands must be two numbers or two strings"))
                }
            }
            TokenType::Greater => binary_number_op!(left, >, right, LoxValue::Bool, err()),
            TokenType::GreaterEqual => binary_number_op!(left, >=, right, LoxValue::Bool, err()),
            TokenType::Less => binary_number_op!(left, <, right, LoxValue::Bool, err()),
            TokenType::LessEqual => binary_number_op!(left, <=, right, LoxValue::Bool, err()),
            TokenType::BangEqual => Ok(LoxValue::Bool(!Self::is_equal(&left, &right))),
            TokenType::EqualEqual => Ok(LoxValue::Bool(Self::is_equal(&left, &right))),

            _ => {
                unreachable!()
            }
        }
    }

    fn visit_unary_expr(&mut self, exp: &expr::Unary) -> Result<LoxValue, LoxError> {
        let right = self.evaluate(&exp.right)?;
        match exp.op.ty {
            TokenType::Minus => {
                if let LoxValue::Number(n) = right {
                    Ok(LoxValue::Number(-n))
                } else {
                    Err(LoxError::Runtime{token: exp.op.clone(), msg: "Operand must be a number".into()})
                    // Err(RuntimeError::new(&exp.op, "Operand must be a number")
                }
            }
            TokenType::Bang => {
                Ok(LoxValue::Bool(!Interpreter::is_truthy(&right)))
            }
            _ => unreachable!()
        }
    }

    fn visit_grouping_expr(&mut self, exp: &expr::Grouping) -> Result<LoxValue, LoxError> {
        self.evaluate(&exp.expression)
    }

    fn visit_variable_expr(&mut self, exp: &expr::Variable) -> Result<LoxValue, LoxError> {
        self.look_up_variable(&exp.name, exp)
    }

    fn visit_assign_expr(&mut self, exp: &expr::Assign) -> Result<LoxValue, LoxError> {
        let value = self.evaluate(&exp.value)?;
        if let Some(distance) = self.locals.get(&exp.name) {
            self.env.as_mut().unwrap().assign_at(*distance, &exp.name, value.clone())?;
        } else {
            self.globals.assign(&exp.name, value.clone())?;
        }
        Ok(value)
    }

    fn visit_logical_expr(&mut self, exp: &expr::Logical) -> Result<LoxValue, LoxError> {
        let left = self.evaluate(&exp.left)?;
        if exp.op.ty == TokenType::Or {
            if Self::is_truthy(&left) {
                return Ok(left);
            }
        } else {
            if !Self::is_truthy(&left) {
                return Ok(left);
            }
        }

        return self.evaluate(&exp.right);
    }

    fn visit_call_expr(&mut self, exp: &expr::Call) -> Result<LoxValue, LoxError> {
        let callee = self.evaluate(&exp.callee)?;
        let mut args = Vec::new();
        for arg in exp.args.iter() {
            args.push(self.evaluate(arg)?);
        }

        let callee = match callee {
            LoxValue::Func(c) => c,
            LoxValue::Class(c) => {
                c.constructor.clone()
            }
            _ => {
                return Err(LoxError::Runtime{token: exp.paren.clone(), msg: "Can only call functions and classes".into()})
            }
        };

        if args.len() > callee.arity {
            return Err(LoxError::Runtime{
                token: exp.paren.clone(),
                msg: format!("Expected {} arguments but got {}", callee.arity, args.len()),
            })
        }
        callee.call(self, args)
    }

    fn visit_get_expr(&mut self, exp: &expr::Get) -> Result<LoxValue, LoxError> {
        let object = self.evaluate(&exp.object)?;
        if let LoxValue::Object(obj) = object {
            return Object::get(obj.clone(), &exp.name);
            // return obj.get(&exp.name);
        }

        Err(LoxError::Runtime{token: exp.name.clone(), msg: "Only instances have properties".into()})
    }

    fn visit_set_expr(&mut self, exp: &expr::Set) -> Result<LoxValue, LoxError> {
        let object = self.evaluate(&exp.object)?;
        if let LoxValue::Object(obj) = object {
            let value = self.evaluate(&exp.value)?;
            obj.set(exp.name.clone(), value.clone());
            return Ok(value);
        }

        Err(LoxError::Runtime{token: exp.name.clone(), msg: "Only instances have fields".into()})
    }

    fn visit_this_expr(&mut self, exp: &expr::This) -> Result<LoxValue, LoxError> {
        self.look_up_variable(&exp.name, exp)
    }

    fn visit_sup_expr(&mut self, exp: &expr::Sup) -> Result<LoxValue, LoxError> {
        let distance = self.locals.get(&exp.name).unwrap();
        let mut tok = Token {
            ty: TokenType::Super,
            lexeme: "super".into(),
            literal: None,
            line: 0,
        };
        let LoxValue::Class(superclass) = self.env.as_ref().unwrap().get_at(*distance, &tok)? else {
            panic!();
        };
        tok.lexeme = "this".into();
        let LoxValue::Object(obj) = self.env.as_ref().unwrap().get_at(distance - 1, &tok)? else {
            panic!();
        };

        if let Some(method) = superclass.find_method(&exp.method.lexeme) {
            return Ok(LoxValue::Func(method.bind(obj)));
        }

        Err(LoxError::Runtime{token: exp.method.clone(), msg: format!("Undefined property '{}'", exp.method.lexeme)})

    }
}

struct Environment {
    values: *mut HashMap<String, LoxValue>,
    enclosing: Option<Box<Environment>>,
    copies: Rc<RefCell<usize>>,
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut count = 0;
        let mut parent = &self.enclosing;
        while let Some(par) = parent {
            count += 1;
            parent = &par.enclosing;
        }

        writeln!(f, "Environment {} {{", count)?;
        for (k, v) in unsafe {self.values.as_ref().unwrap().iter()} {
            writeln!(f, "  {}: {}", k, v)?;
        }
        write!(f, "}}")
    }
}

impl Clone for Environment {
    fn clone(&self) -> Environment {
        *self.copies.borrow_mut() += 1;
        Environment {
            values: self.values,
            enclosing: self.enclosing.clone(),
            copies: self.copies.clone(),
        }
    }
}

impl Drop for Environment {
    fn drop(&mut self) {
        if *self.copies.borrow() == 1 {
            unsafe {
                drop(Box::from_raw(self.values)); // just making explicit
            }
        } else {
            *self.copies.borrow_mut() -= 1;
        }
    }
}

impl Environment {
    fn new() -> Box<Environment> {
        let values = Box::new(HashMap::new());
        Box::new(Environment {
            values: Box::into_raw(values),
            enclosing: None,
            copies: Rc::new(RefCell::new(1)),
        })
    }

    fn enclosing(env: Box<Environment>) -> Box<Environment> {
        let values = Box::new(HashMap::new());
        Box::new(Environment {
            values: Box::into_raw(values),
            enclosing: Some(env),
            copies: Rc::new(RefCell::new(1)),
        })
    }

    fn define(&mut self, name: &str, value: LoxValue) {
        unsafe {
            self.values.as_mut().unwrap().insert(name.to_string(), value);
        }
    }

    fn get(&self, name: &Token) -> Result<LoxValue, LoxError> {
        let value = unsafe {self.values.as_ref().unwrap().get(&name.lexeme)};
        match value {
            Some(v) => Ok(v.clone()),
            None => match self.enclosing {
                Some(ref enc) => enc.get(name),
                None => Err(LoxError::Runtime{token: name.clone(), msg: format!("Undefined variable '{}'", name.lexeme)})
            }
        }
    }

    // This should technically not be a Token, but everything plays nicer if it is.
    fn get_at(&self, distance: usize, name: &Token) -> Result<LoxValue, LoxError> {
        if distance == 0 {
            let value = unsafe {self.values.as_ref().unwrap().get(&name.lexeme)};
            match value {
                Some(v) => Ok(v.clone()),
                None => Err(LoxError::Runtime{token: name.clone(), msg: format!("Undefined variable '{}'", name.lexeme)})
            }
        } else {
            self.enclosing.as_ref().unwrap().get_at(distance - 1, name)
        }
    }

    fn assign(&mut self, name: &Token, value: LoxValue) -> Result<(), LoxError> {
        if unsafe{self.values.as_ref().unwrap().contains_key(&name.lexeme)} {
            unsafe {self.values.as_mut().unwrap().insert(name.lexeme.clone(), value);}
            Ok(())
        } else if let Some(ref mut env) = self.enclosing {
            env.assign(name, value)
        } else {
            Err(LoxError::Runtime{token: name.clone(), msg: format!("Undefined variable '{}'", name.lexeme)})
            // Err(RuntimeError::new(&name, &format!("Undefined variable '{}'", name.lexeme)))
        }
    }

    fn assign_at(&mut self, distance: usize, name: &Token, value: LoxValue) -> Result<(), LoxError> {
        if distance == 0 {
            if unsafe{self.values.as_ref().unwrap().contains_key(&name.lexeme)} {
                unsafe {self.values.as_mut().unwrap().insert(name.lexeme.clone(), value);}
                Ok(())
            } else {
                Err(LoxError::Runtime{token: name.clone(), msg: format!("Undefined variable '{}'", name.lexeme)})
            }
        } else {
            self.enclosing.as_mut().unwrap().assign_at(distance - 1, name, value)
        }
    }

    // fn parent(&mut self) -> Option<Box<Environment>> {
    //     self.enclosing.take()
    // }
}
