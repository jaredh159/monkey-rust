use std::io;

mod lexer;
mod parser;
mod repl;
mod token;

// token
// lexer
// parser
//  -> ast
//  -> expr
//  -> statement

fn main() {
  println!("This is the Monkey programming language!");
  println!("Feel free to type in commands");
  let stdout = io::stdout();
  repl::start(io::stdin(), stdout);
}
