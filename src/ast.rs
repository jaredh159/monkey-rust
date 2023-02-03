use crate::token::*;

pub type Program = Vec<Statement>;

impl Node for Program {
  fn token_literal(&self) -> String {
    match self.first() {
      Some(stmt) => stmt.token_literal(),
      None => "".to_string(),
    }
  }
}

#[derive(Debug)]
pub enum Expr {
  Ident(Identifier),
  Todo,
}

impl Node for Expr {
  fn token_literal(&self) -> String {
    match self {
      Expr::Ident(ident) => ident.token.literal(),
      Expr::Todo => String::new(),
    }
  }
}

#[derive(Debug)]
pub struct Identifier {
  pub token: Token,
  pub value: String,
}

impl Node for Identifier {
  fn token_literal(&self) -> String {
    self.token.literal()
  }
}

#[derive(Debug)]
pub enum Statement {
  Let(Token, Identifier, Expr),
  Return,
}

impl Node for Statement {
  fn token_literal(&self) -> String {
    match self {
      Statement::Let(token, _, _) => token.literal(),
      Statement::Return => "TODO".to_string(),
    }
  }
}

pub trait Node {
  fn token_literal(&self) -> String;
}
