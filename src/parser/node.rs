use crate::parser::stmt::Statement;

pub type Program = Vec<Statement>;

pub trait Node {
  fn token_literal(&self) -> String;
  fn string(&self) -> String;
}

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

// move? todo
#[cfg(test)]
mod tests {
  use crate::parser::expr::*;
  use crate::parser::node::*;
  use crate::token::*;

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
