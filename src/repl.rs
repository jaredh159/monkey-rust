use crate::lexer::Lexer;
use crate::parser::*;
use crate::{eval::eval, eval::Node, object::Object};
use std::io::{BufRead, Stdin, Stdout, Write};

pub fn start(stdin: Stdin, mut stdout: Stdout) {
  print!(">> ");
  stdout.flush().unwrap();

  for line in stdin.lock().lines() {
    let line = line.unwrap();
    let lexer = Lexer::new(line);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    if parser.errors.len() > 0 {
      for err in parser.errors {
        eprintln!("  -> {}", err.message());
      }
      continue;
    }
    println!("{}", eval(Node::Prog(program)).inspect());
    print!(">> ");
    stdout.flush().unwrap();
  }
}
