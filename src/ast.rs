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

#[derive(Clone, Debug)]
pub struct IfExpression {
  pub token: Token,
  pub condition: Box<Expr>,
  pub consequence: BlockStatement,
  pub alternative: Option<BlockStatement>,
}

#[derive(Clone, Debug)]
pub struct FunctionLiteral {
  pub token: Token,
  pub parameters: Vec<Identifier>,
  pub body: BlockStatement,
}

#[derive(Clone, Debug)]
pub enum Expr {
  Ident(Identifier),
  IntegerLiteral(Token, i64),
  BooleanLiteral(Token, bool),
  Prefix(Token, String, Box<Expr>),
  Infix(Token, Box<Expr>, String, Box<Expr>),
  If(IfExpression),
  Function(FunctionLiteral),
  Todo,
}

impl Node for Expr {
  fn token_literal(&self) -> String {
    match self {
      Expr::Ident(ident) => ident.token.literal(),
      Expr::IntegerLiteral(token, _) => token.literal(),
      Expr::Prefix(token, _, _) => token.literal(),
      Expr::Infix(token, _, _, _) => token.literal(),
      Expr::BooleanLiteral(token, _) => token.literal(),
      Expr::If(if_expr) => if_expr.token.literal(),
      Expr::Function(fn_literal) => fn_literal.token.literal(),
      Expr::Todo => String::from("TODO"),
    }
  }

  fn string(&self) -> String {
    match self {
      Expr::Ident(ident) => ident.string(),
      Expr::IntegerLiteral(token, _) => token.literal(),
      Expr::BooleanLiteral(token, _) => token.literal(),
      Expr::Prefix(_, operator, expr) => format!("({}{})", operator, expr.string()),
      Expr::Infix(_, lhs, operator, rhs) => {
        format!("({} {} {})", lhs.string(), operator, rhs.string())
      }
      Expr::Function(fn_literal) => {
        format!(
          "{} ({}) {}",
          fn_literal.token.literal(),
          fn_literal
            .parameters
            .iter()
            .map(|p| p.string())
            .collect::<Vec<_>>()
            .join(", "),
          fn_literal.body.string()
        )
      }
      Expr::If(if_expr) => {
        let mut string = format!(
          "if {} {}",
          if_expr.condition.string(),
          if_expr.consequence.string()
        );
        if let Some(alternative) = &if_expr.alternative {
          string.push_str(&format!("else {}", alternative.string()));
        }
        string
      }
      Expr::Todo => String::from("TODO"),
    }
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct BlockStatement {
  pub token: Token,
  pub statements: Vec<Statement>,
}

impl Node for BlockStatement {
  fn token_literal(&self) -> String {
    self.token.literal()
  }

  fn string(&self) -> String {
    self
      .statements
      .iter()
      .map(|stmt| stmt.string())
      .collect::<Vec<String>>()
      .join("")
  }
}

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
