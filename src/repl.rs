use crate::ast::Node;
use crate::{lexer::Lexer, parser::Parser};
use std::io::{BufRead, Stdin, Stdout, Write};

pub fn start(stdin: Stdin, mut stdout: Stdout) {
  print!(">> ");
  stdout.flush().unwrap();

  for line in stdin.lock().lines() {
    let line = line.unwrap();
    let lexer = Lexer::new(line);
    let mut parser = Parser::new(lexer);
    for statement in parser.parse_program() {
      println!("{:?}", statement.string());
    }
    print!(">> ");
    stdout.flush().unwrap();
  }
}
