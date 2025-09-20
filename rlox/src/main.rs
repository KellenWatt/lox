use std::env;
use std::process::ExitCode;
use anyhow::Result;

mod scanner;
mod parser;
use crate::parser::Parser;
mod error;
mod expr;
mod stmt;
use crate::scanner::Scanner;

mod visitor;
use crate::visitor::interpreter::Interpreter;
use crate::visitor::resolver::Resolver;

// fn main() {
//     use crate::expr::{self, Expr};
//     use crate::scanner::{Token, TokenType, Literal};
//     let expression = Expr::binary(
//         Expr::unary(
//             Token {
//                 ty: TokenType::Minus,
//                 lexeme: "-".into(),
//                 literal: None, 
//                 line: 1,
//             },
//             Expr::literal(Literal::Number(123.))
//         ),
//         Token {
//             ty: TokenType::Star,
//             lexeme: "*".into(),
//             literal: None,
//             line: 1,
//         },
//         Expr::grouping(Expr::literal(Literal::Number(45.67)))
//     );
//     println!("{}", crate::visitor::printer::Printer.print(&expression));
// }

fn main() -> Result<ExitCode> {
    let args = env::args().collect::<Vec<String>>();
    if args.len() > 2 {
        println!("Usage: rlox [SCRIPT]");
        return Ok(ExitCode::from(64));
    }
    if args.len() == 1 {
        run_prompt()?;
    } else {
        return run_file(&args[1]);
    }
    Ok(ExitCode::SUCCESS)
}

fn run_file(file: &str) -> Result<ExitCode> {
    let mut interpreter = Interpreter::new();
    let code = std::fs::read_to_string(file)?;

    run(&code, &mut interpreter)?;
    if error::has_errored() {
        Ok(ExitCode::from(65))
    } else if error::has_runtime_errored() {
        Ok(ExitCode::from(70))
    }else {
        Ok(ExitCode::SUCCESS)
    }

}

fn run_prompt() -> Result<()> {
    use rustyline::DefaultEditor;
    use rustyline::error::ReadlineError;
    let mut rl = DefaultEditor::new()?;

    let mut interpreter = Interpreter::new();

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                run(&line, &mut interpreter)?;
                error::clear_error();
            }
            Err(ReadlineError::Interrupted) => {
                println!("Ctrl-C pressed");
                continue;
            }
            Err(ReadlineError::Eof) => {
                break
            }
            Err(e) => {
                return Err(e.into());
            }
        }
    }

    Ok(())
}

fn run(code: &str, interpreter: &mut Interpreter) -> Result<()> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.scan_all().clone();
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse();

    if error::has_errored() {return Ok(());}

    let mut resolver = Resolver::new(interpreter);
    resolver.resolve_stmt_list(&stmts);

    if error::has_errored() {return Ok(());}
  
    interpreter.interpret(stmts);

    Ok(())
}
