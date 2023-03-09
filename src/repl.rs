use crate::lexer::Lexer;
use crate::object::{Env, Object};
use crate::parser::*;
use crate::{eval::eval, eval::Node};
use std::cell::RefCell;
use std::io::{BufRead, Stdin, Stdout, Write};
use std::rc::Rc;

pub fn start(stdin: Stdin, mut stdout: Stdout) {
  print!(">> ");
  stdout.flush().unwrap();
  let env = Rc::new(RefCell::new(Env::new()));

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
    println!("{}", eval(Node::Prog(program), Rc::clone(&env)).inspect());
    print!(">> ");
    stdout.flush().unwrap();
  }
}
