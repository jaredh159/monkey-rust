use crate::token::*;

pub type Program = Vec<Statement>;

impl Node for Program {
  fn token_literal(&self) -> String {
    match self.first() {
      Some(stmt) => stmt.token_literal(),
      None => "".to_string(),
    }
  }

  fn string(&self) -> String {
    let mut string = String::new();
    for statement in self {
      string.push_str(&statement.string());
    }
    return string;
  }
}

#[derive(Debug)]
pub enum Expr {
  Ident(Identifier),
  IntegerLiteral(Token, i64),
  Prefix(Token, String, Box<Expr>),
  Todo,
}

impl Node for Expr {
  fn token_literal(&self) -> String {
    match self {
      Expr::Ident(ident) => ident.token.literal(),
      Expr::IntegerLiteral(token, _) => token.literal(),
      Expr::Prefix(token, _, _) => token.literal(),
      Expr::Todo => String::from("TODO"),
    }
  }

  fn string(&self) -> String {
    match self {
      Expr::Ident(ident) => ident.string(),
      Expr::IntegerLiteral(token, _) => token.literal(),
      Expr::Prefix(_, operator, expr) => format!("({}{})", operator, expr.string()),
      Expr::Todo => String::from("TODO"),
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

  fn string(&self) -> String {
    self.value.clone()
  }
}

#[derive(Debug)]
pub enum Statement {
  Let(Token, Identifier, Expr),
  Return(Token, Expr),
  Expression(Token, Expr),
}

impl Node for Statement {
  fn token_literal(&self) -> String {
    match self {
      Statement::Let(token, _, _) => token.literal(),
      Statement::Return(token, _) => token.literal(),
      Statement::Expression(token, _) => token.literal(),
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
    }
  }
}

pub trait Node {
  fn token_literal(&self) -> String;
  fn string(&self) -> String;
}

#[cfg(test)]
mod tests {
  use crate::ast::*;

  #[test]
  fn token_string() {
    let program: Program = vec![Statement::Let(
      Token::Let,
      Identifier {
        token: Token::Ident("myVar".to_string()),
        value: "myVar".to_string(),
      },
      Expr::Ident(Identifier {
        token: Token::Ident("anotherVar".to_string()),
        value: "anotherVar".to_string(),
      }),
    )];

    assert_eq!(program.string(), "let myVar = anotherVar;");
  }
}
