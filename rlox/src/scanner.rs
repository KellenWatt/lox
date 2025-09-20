use std::fmt::{self, Display};
use crate::error;

#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
    Ident(String),
    String(String),
    Number(f64),
    Bool(bool),
    Nil, 
}
impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;
        match self {
            Ident(name) => write!(f, "{}", name),
            String(value) => write!(f, "{}", value),
            Number(value) => write!(f, "{}", value),
            Bool(value) => write!(f, "{}", value),
            Nil => write!(f, "nil")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
#[allow(dead_code)]
pub enum TokenType {
    // Single-char
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    
    // 1-2 char 
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Ident,
    Str,
    Number,

    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    EOF,
}
impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub ty: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
}

impl Eq for Token {} // This isn't strictly true, but I don't care about tokens that contain literals

impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
        self.lexeme.hash(state);
        self.line.hash(state);
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.literal {
            Some(lit) => write!(f, "{} {} {}", self.ty, self.lexeme, lit),
            None => write!(f, "{} {}", self.ty, self.lexeme),
        }
    }
}

fn is_alpha(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}
fn is_alnum(c: char) -> bool {
    is_alpha(c) || c.is_digit(10)
}

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: source.chars().collect(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan(&mut self) {
        // This line was in scan_all, above the call to scan. Put it back if it breaks anything
        self.start = self.current;
        let Some(c) = self.advance() else {return;};
        use TokenType::*;
        match c {
            '(' => self.add_token(LeftParen, None),
            ')' => self.add_token(RightParen, None),
            '{' => self.add_token(LeftBrace, None),
            '}' => self.add_token(RightBrace, None),
            ',' => self.add_token(Comma, None),
            '.' => self.add_token(Dot, None),
            '-' => self.add_token(Minus, None),
            '+' => self.add_token(Plus, None),
            ';' => self.add_token(Semicolon, None),
            '*' => self.add_token(Star, None),
            '!' => if self.r#match('=') {
                    self.add_token(BangEqual, None);
                } else {
                    self.add_token(Bang, None)
                }
            '=' => if self.r#match('=') {
                    self.add_token(EqualEqual, None);
                } else {
                    self.add_token(Equal, None)
                }
            '<' => if self.r#match('=') {
                    self.add_token(LessEqual, None);
                } else {
                    self.add_token(Less, None)
                }
            '>' => if self.r#match('=') {
                    self.add_token(GreaterEqual, None);
                } else {
                    self.add_token(Greater, None)
                }
            '/' => if self.r#match('/') {
                    // A comment
                    while self.peek() != '\n' && !self.is_finished() {let _ = self.advance();}
                } else {
                    self.add_token(Slash, None);
                }
            '"' => {
                self.string();
            }
            ' ' | '\r' | '\t' => {
                // Do nothing
            }
            '\n' => {
                self.line += 1;
            }
            _ if c.is_digit(10) => {
                self.number();
            }
            _ if is_alpha(c) => {
                self.identifier();
            }
            _ => {
                error::error(self.line, &format!("Unexpected character: {}", c));
            }
        }
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_finished() {
            if self.peek() == '\n' { self.line += 1; }
            self.advance();
        }

        if self.is_finished() {
            error::error(self.line, "Unterminated string");
            return;
        }

        self.advance(); // closing "
        
        let value = self.source[(self.start+1)..(self.current-1)].iter().collect();
        self.add_token(TokenType::Str, Some(Literal::String(value)));
    }

    fn number(&mut self) {
        self.advance_while(|c| c.is_digit(10));
        if self.peek() == '.' && self.peekn(1).is_digit(10) {
            // consume the '.'
            let _ = self.advance();
            self.advance_while(|c| c.is_digit(10));
        }
        let num = self.source[self.start..self.current].iter().collect::<String>().parse().unwrap();
        self.add_token(TokenType::Number, Some(Literal::Number(num)));
    }

    fn identifier(&mut self) {
        self.advance_while(|c| is_alnum(c));
        let ident: String = self.source[self.start..self.current].iter().collect();
       
        use TokenType::*;
        let ty = match &*ident {
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            _ => Ident,
        };

        if ty == Ident {
            self.add_token(ty, Some(Literal::Ident(ident)));
        } else {
            self.add_token(ty, None);
        }
    }

    pub fn scan_all(&mut self) -> &Vec<Token> {
        while !self.is_finished() {
            self.scan();
        }
        self.tokens.push(Token {
            ty: TokenType::EOF,
            lexeme: String::new(),
            literal: None,
            line: self.line,
        });
        &self.tokens
    }

    fn advance(&mut self) -> Option<char> {
        if self.is_finished() {
            return None;
        }
        self.current += 1;
        Some(self.source[self.current - 1])
    }

    fn advance_while<F: Fn(char) -> bool>(&mut self, pred: F) {
        while pred(self.peek()) {
            let _ = self.advance();
        }
    }

    fn peek(&self) -> char {
        self.peekn(0)
    }

    fn peekn(&self, n: usize) -> char {
        if self.current + n >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + n]
        }
    }

    fn r#match (&mut self, c: char) -> bool {
        if self.peek() == c {
            self.current += 1;
            return true;
        }
        false
    }

    fn add_token(&mut self, ty: TokenType, lit: Option<Literal>) {
        let text = self.source[self.start..self.current].iter().collect();
        self.tokens.push(Token {
            ty,
            lexeme: text,
            literal: lit,
            line: self.line,
        })
    }

    fn is_finished(&self) -> bool {
        self.current >= self.source.len()
    }
}

// FIXME Doing this requires that scan return a Token. 
//       Not difficult, but also not how things are set up right now.
// impl Iterator for Scanner {
//     type Item = Token;
//     fn next(&mut self) -> Option<Self::Item> {
//         if self.is_finished() {
//             return None;
//         }
//         self.scan();
//         self.tokens.last().cloned()
//     }
// }
