use crate::parser::stmt::Statement;
use crate::token::Token;

pub type Program = Vec<Statement>;

pub trait Node {
  fn token_literal(&self) -> String;
  fn string(&self) -> String;
}

pub trait TokenNode {
  fn token(&self) -> &Token;
}

impl<T> Node for T
where
  T: TokenNode,
{
  fn token_literal(&self) -> String {
    self.token().literal()
  }
  fn string(&self) -> String {
    self.token_literal()
  }
}

impl Node for Program {
  fn token_literal(&self) -> String {
    self.first().map_or_else(String::new, Node::token_literal)
  }

  fn string(&self) -> String {
    let mut string = String::new();
    for statement in self {
      string.push_str(&statement.string());
    }
    string
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
