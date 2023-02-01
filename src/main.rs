use std::io;

mod lexer;
mod repl;
mod token;

fn main() {
  println!("This is the Monkey programming language!");
  println!("Feel free to type in commands");
  let stdout = io::stdout();
  repl::start(io::stdin(), stdout);
}
