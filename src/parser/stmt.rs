use crate::parser::expr::{BlockStatement, Expr, Identifier};
use crate::parser::node::Node;
use crate::token::Token;

#[derive(Clone, Debug)]
pub enum Statement {
  Let(Token, Identifier, Expr),
  Return(Token, Expr),
  Expression(Token, Expr),
  Block(BlockStatement),
}

impl Node for Statement {
  fn token_literal(&self) -> String {
    match self {
      Statement::Let(token, _, _) => token.literal(),
      Statement::Return(token, _) => token.literal(),
      Statement::Expression(token, _) => token.literal(),
      Statement::Block(block) => block.token.literal(),
    }
  }

  fn string(&self) -> String {
    match self {
      Statement::Let(token, name, val) => {
        format!("{} {} = {};", token.literal(), name.string(), val.string())
      }
      Statement::Return(token, value) => {
        format!("{} {};", token.literal(), value.string())
      }
      Statement::Expression(_, expr) => expr.string(),
      Statement::Block(block_stmt) => block_stmt.string(),
    }
  }
}
