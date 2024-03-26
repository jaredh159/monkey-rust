use crate::lexer::Lexer;
use crate::object::{Env, Object};
use crate::parser::*;
use crate::{eval::eval, eval::Node};
use std::cell::RefCell;
use std::io::{Stdin, Stdout, Write};
use std::rc::Rc;

pub fn start(stdin: Stdin, mut stdout: Stdout) {
  print!(">> ");
  stdout.flush().unwrap();
  let env = Rc::new(RefCell::new(Env::new()));

  let lines = stdin.lines();
  for line in lines {
    let line = line.unwrap();
    let lexer = Lexer::new(&line);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    if !parser.errors.is_empty() {
      for err in parser.errors {
        eprintln!("  -> {}", err.message());
      }
      continue;
    }
    println!("{}", eval(Node::Prog(program), Rc::clone(&env)).inspect());
    print!(">> ");
    stdout.flush().unwrap();
  }
}
