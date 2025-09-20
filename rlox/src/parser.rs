
use crate::scanner::{Token, TokenType, Literal};
use crate::expr::{self, Expr};
use crate::stmt::{Stmt};
use crate::error::{self, ParseError};
use std::rc::Rc;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while !self.is_finished() {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt);
            }
        }

        statements
    }

    fn declaration(&mut self) -> Option<Stmt> {
        let out = if self.r#match(&[TokenType::Var]) {
            self.var_declaration()
        } else if self.r#match(&[TokenType::Fun]) {
            self.function("function")
        } else if self.r#match(&[TokenType::Class]) {
            self.class_declaration()
        } else {
            self.statement()
        };
        match out {
            Ok(s) => Some(s),
            Err(_) => {
                self.synchronize();
                None
            }
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Ident, "Expect class name")?.clone();

        let mut superclass = None;
        if self.r#match(&[TokenType::Less]) {
            self.consume(TokenType::Ident, "Expect superclass name")?;
            superclass = Some(expr::Variable{name: self.previous().unwrap().clone()});
        }

        self.consume(TokenType::LeftBrace, "Expect '{' before class body")?;

        let mut methods = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_finished() {
            match self.function("method") {
                Ok(Stmt::Function(f)) => methods.push(f),
                Err(e) => return Err(e),
                _ => unreachable!()
            }
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body")?;
        Ok(Stmt::class(name, superclass, methods))
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Ident, "Expect variable name")?.clone();
        let mut initializer = None;
        if self.r#match(&[TokenType::Equal]) {
            let _ = initializer.insert(self.expression()?);
        }

        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration")?;
        Ok(Stmt::var(name, initializer))
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Ident, &format!("Expect {} name", kind))?.clone();

        self.consume(TokenType::LeftParen, &format!("Expect '(' after {} name", kind))?;
        let mut params = Vec::new();
        if !self.check(&TokenType::RightParen) {
            params.push(self.consume(TokenType::Ident, "Expect parameter name")?.clone());
            while self.r#match(&[TokenType::Comma]) {
                if params.len() >= 255 {
                    error::token_error(self.peek().unwrap(), "Can't have more than 255 parameters");
                }
                params.push(self.consume(TokenType::Ident, "Expect parameter name")?.clone());
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters")?;

        // This is the body. I would like this to be a single statement, but we'll see
        self.consume(TokenType::LeftBrace, &format!("Expect '{{' before {} body", kind))?;
        let body = self.block()?;
        Ok(Stmt::function(name.clone(), params, Rc::new(body)))

    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.r#match(&[TokenType::Print]) {
            self.print_statement()
        } else if self.r#match(&[TokenType::LeftBrace]) {
            Ok(Stmt::block(self.block()?))
        } else if self.r#match(&[TokenType::If]) {
            self.if_statement()
        } else if self.r#match(&[TokenType::While]) {
            self.while_statement()
        } else if self.r#match(&[TokenType::For]) {
            self.for_statement()
        } else if self.r#match(&[TokenType::Return]) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }
    
    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value")?;
        Ok(Stmt::print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression")?;
        Ok(Stmt::expression(expr))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_finished() {
            if let Some(decl) = self.declaration() {
                statements.push(decl);
            }
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block")?;
        Ok(statements)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition")?;

        let then_branch = self.statement()?;
        let mut else_branch = None;
        if self.r#match(&[TokenType::Else]) {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Stmt::r#if(condition, Box::new(then_branch), else_branch))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition")?;
        let body = self.statement()?;

        Ok(Stmt::r#while(condition, Box::new(body)))
    }
    
    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'")?;

        let mut initializer = None;
        if self.r#match(&[TokenType::Var]) {
            initializer = Some(self.var_declaration()?);
        } else if !self.r#match(&[TokenType::Semicolon]) {
            initializer = Some(self.expression_statement()?);
        }

        let condition = if !self.check(&TokenType::Semicolon) {
            self.expression()?
        } else {
            Expr::literal(Literal::Bool(true))
        };
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition")?;

        let mut increment = None;
        if !self.check(&TokenType::RightParen) {
            increment = Some(self.expression()?);
        }
        self.consume(TokenType::RightParen, "Expect ')' after for clauses")?;

        let mut body = self.statement()?;
        
        if let Some(inc) = increment {
            body = Stmt::block(vec![body, Stmt::expression(inc)]);
        }
        body = Stmt::r#while(condition, Box::new(body));
        if let Some(init) = initializer {
            body = Stmt::block(vec![init, body]);
        }

        Ok(body)
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let keyword = self.previous().unwrap().clone();
        let mut value = None;
        if !self.check(&TokenType::Semicolon) {
            value = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon, "Expect ';' after return value")?;
        Ok(Stmt::r#return(keyword, value))
    }

    fn expression(&mut self) -> Result<Box<Expr>, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Box<Expr>, ParseError> {
        let exp = self.or()?;
        if self.r#match(&[TokenType::Equal]) {
            let equals = self.previous().unwrap().clone();
            let value = self.assignment()?;

            if let Expr::Variable(var) = *exp {
                let name = var.name;
                return Ok(Expr::assign(name, value));
            } else if let Expr::Get(get) = *exp {
                return Ok(Expr::set(get.object, get.name, value));
            }

            error::token_error(&equals, "Invalid assignment target");
        }

        Ok(exp)
    }

    fn or(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut exp = self.and()?;

        while self.r#match(&[TokenType::Or]) {
            let op = self.previous().unwrap().clone();
            let right = self.and()?;
            exp = Expr::logical(exp, op, right);
        }

        Ok(exp)
    }
    
    fn and(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut exp = self.equality()?;

        while self.r#match(&[TokenType::And]) {
            let op = self.previous().unwrap().clone();
            let right = self.equality()?;
            exp = Expr::logical(exp, op, right);
        }

        Ok(exp)
    }

    fn equality(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut exp = self.comparison()?;

        while self.r#match(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = self.previous().unwrap().clone();
            let right = self.comparison()?;
            exp = Expr::binary(exp, op, right);
        }

        Ok(exp)
    }

    fn comparison(&mut self) -> Result<Box<Expr>, ParseError> {
        use TokenType::*;
        let mut exp = self.term()?;

        while self.r#match(&[Greater, GreaterEqual, Less, LessEqual]) {
            let op = self.previous().unwrap().clone();
            let right = self.term()?;
            exp = Expr::binary(exp, op, right);
        }
        Ok(exp)
    }

    fn term(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut exp = self.factor()?;
        while self.r#match(&[TokenType::Minus, TokenType::Plus]) {
            let op = self.previous().unwrap().clone();
            let right = self.factor()?;
            exp = Expr::binary(exp, op, right);
        }

        Ok(exp)
    }

    fn factor(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut exp = self.unary()?;
        while self.r#match(&[TokenType::Slash, TokenType::Star]) {
            let op = self.previous().unwrap().clone();
            let right = self.unary()?;
            exp = Expr::binary(exp, op, right);
        }

        Ok(exp)
    }

    fn unary(&mut self) -> Result<Box<Expr>, ParseError> {
        if self.r#match(&[TokenType::Bang, TokenType::Minus]) {
            let op = self.previous().unwrap().clone();
            let right = self.unary()?;
            Ok(Expr::unary(op, right))
        } else {
            Ok(self.call()?)
        }
    }

    fn call(&mut self) -> Result<Box<Expr>, ParseError> {
        let mut exp = self.primary()?;

        loop {
            if self.r#match(&[TokenType::LeftParen]) {
                exp = self.finish_call(exp)?;
            } else if self.r#match(&[TokenType::Dot]) {
                let name = self.consume(TokenType::Ident, "Expect property name after '.'")?.clone();
                exp = Expr::get(exp, name);
            } else {
                break;
            }
        }

        Ok(exp)
    }

    fn finish_call(&mut self, callee: Box<Expr>) -> Result<Box<Expr>, ParseError> {
        let mut args = Vec::new();
        if !self.check(&TokenType::RightParen) {
            args.push(self.expression()?);
            while self.r#match(&[TokenType::Comma]) {
                if args.len() >= 255 {
                    error::token_error(self.peek().unwrap(), "Can't have more than 255 arguments");
                }
                args.push(self.expression()?);
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments")?;

        Ok(Expr::call(callee, paren.clone(), args))
    }

    fn primary(&mut self) -> Result<Box<Expr>, ParseError> {
        Ok(if self.r#match(&[TokenType::False]) {
            Expr::literal(Literal::Bool(false))
        } else if self.r#match(&[TokenType::True]) {
            Expr::literal(Literal::Bool(true))
        } else if self.r#match(&[TokenType::Nil]) {
            Expr::literal(Literal::Nil)
        } else if self.r#match(&[TokenType::Number]) {
            Expr::literal(self.previous().unwrap().literal.clone().unwrap())
        } else if self.r#match(&[TokenType::Str]) {
            Expr::literal(self.previous().unwrap().literal.clone().unwrap())
        } else if self.r#match(&[TokenType::Ident]) {
            Expr::variable(self.previous().unwrap().clone())
        } else if self.r#match(&[TokenType::LeftParen]) {
            let exp = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            Expr::grouping(exp)
        } else if self.r#match(&[TokenType::This]) {
            Expr::this(self.previous().unwrap().clone())
        } else if self.r#match(&[TokenType::Super]) {
            let keyword = self.previous().unwrap().clone();
            self.consume(TokenType::Dot, "Expect '.' after 'super'")?;
            let method = self.consume(TokenType::Ident, "Expect superclass method name")?.clone();
            Expr::sup(keyword, method)
        } else {
            self.error(self.peek().unwrap(), "Expect expression.")?;
            unreachable!();
        })
    }

    fn r#match(&mut self, types: &[TokenType]) -> bool {
        for ty in types {
            if self.check(ty) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, ty: TokenType, msg: &str) -> Result<&Token, ParseError> {
        if self.check(&ty) {
            Ok(self.advance())
        } else {
            let tok = self.peek().unwrap();
            self.error(tok, msg)?;
            unreachable!()
        }
    }

    fn check(&self, ty: &TokenType) -> bool {
        if self.is_finished() {
            return false;
        }
        self.peek().map(|tok| tok.ty == *ty).unwrap_or(false)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_finished() {
            self.current += 1;
        }
        self.previous().unwrap()
    }

    fn is_finished(&self) -> bool {
        let Some(tok) = self.peek() else {
            return false;
        };
        tok.ty == TokenType::EOF
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn error(&self, tok: &Token, msg: &str) -> Result<(), ParseError> {
        error::token_error(tok, msg);
        Err(ParseError::new(msg))
    }

    fn synchronize(&mut self) {
        use TokenType::*;
        let _ = self.advance();
        while !self.is_finished() {
            if self.previous().unwrap().ty == Semicolon {
                return;
            }
            match self.peek().unwrap().ty {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => self.advance(),
            };
        }
    }
}
