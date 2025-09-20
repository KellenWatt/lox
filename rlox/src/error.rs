#![allow(dead_code)]
use std::sync::atomic::{AtomicBool, Ordering};

static HAD_ERROR: AtomicBool = AtomicBool::new(false);
static HAD_RUNTIME_ERROR: AtomicBool = AtomicBool::new(false);

pub fn report(line: usize, loc: &str, message: &str) {
    if loc.len() > 0 {
        eprintln!("[line {}] Error {}: {}", line, loc, message)
    } else {
        eprintln!("[line {}] Error: {}", line, message)
    }
    HAD_ERROR.store(true, Ordering::Release);
}

pub fn error(line: usize, message: &str) {
    report(line, "", message);
}

pub fn runtime_error(err: RuntimeError) {
    eprintln!("{}\n[line {}]", err.msg, err.token.line);
    HAD_RUNTIME_ERROR.store(true, Ordering::Release);
}

pub fn token_error(tok: &crate::scanner::Token, msg: &str) {
    if tok.ty == crate::scanner::TokenType::EOF {
        report(tok.line, "at end", msg);
    } else {
        report(tok.line, &format!("at '{}'", tok.lexeme), msg);
    }
}

pub fn has_errored() -> bool {
    HAD_ERROR.load(Ordering::Acquire)
}

pub fn has_runtime_errored() -> bool {
    HAD_RUNTIME_ERROR.load(Ordering::Acquire)
}

pub fn clear_error() {
    HAD_ERROR.store(false, Ordering::Release);
}

pub fn clear_runtime_error() {
    HAD_RUNTIME_ERROR.store(false, Ordering::Release);
}

#[derive(Debug)]
pub struct ParseError {
    msg: String,
}

impl ParseError {
    pub fn new(msg: &str) -> ParseError {
        ParseError {
            msg: msg.to_string()
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ParseError: {}", self.msg)
    }
}

impl std::error::Error for ParseError {}


#[derive(Debug)]
pub struct RuntimeError {
    msg: String,
    token: crate::scanner::Token,
}

impl RuntimeError {
    pub fn new(tok: &crate::scanner::Token, msg: &str) -> RuntimeError {
        RuntimeError {
            msg: msg.to_string(),
            token: tok.clone(),
        }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::error::Error for RuntimeError {}
