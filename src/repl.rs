use crate::lexer::Lexer;
use crate::token::Token;
use std::io::{BufRead, Stdin, Stdout, Write};

pub fn start(stdin: Stdin, mut stdout: Stdout) {
  print!(">> ");
  stdout.flush().unwrap();

  for line in stdin.lock().lines() {
    let line = line.unwrap();
    let lexer = Lexer::new(line);
    for token in lexer {
      if token == Token::EOF {
        break;
      }
      println!("{:?}", token);
    }
    print!(">> ");
    stdout.flush().unwrap();
  }
}
