use crate::parser::expr::{Expr, Identifier};
use crate::parser::node::Node;
use crate::token::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
  Let(Token, Identifier, Expr),
  Return(Token, Expr),
  Expression(Token, Expr),
  Block(BlockStatement),
}

impl Node for Statement {
  fn token_literal(&self) -> String {
    match self {
      Self::Let(t, _, _) | Self::Return(t, _) | Self::Expression(t, _) => t.literal(),
      Self::Block(block) => block.token.literal(),
    }
  }

  fn string(&self) -> String {
    match self {
      Self::Let(token, name, val) => {
        format!("{} {} = {};", token.literal(), name.string(), val.string())
      }
      Self::Return(token, value) => {
        format!("{} {};", token.literal(), value.string())
      }
      Self::Expression(_, expr) => expr.string(),
      Self::Block(block_stmt) => block_stmt.string(),
    }
  }
}

// BlockStatement

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockStatement {
  pub token: Token,
  pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
  fn token_literal(&self) -> String {
    self.token.literal()
  }

  fn string(&self) -> String {
    self.statements.iter().map(Node::string).collect::<String>()
  }
}
