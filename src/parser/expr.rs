use crate::parser::node::Node;
use crate::parser::stmt::Statement;
use crate::token::*;

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

impl Node for FunctionLiteral {
  fn token_literal(&self) -> String {
    self.token.literal()
  }

  fn string(&self) -> String {
    format!(
      "{} ({}) {}",
      self.token.literal(),
      self
        .parameters
        .iter()
        .map(|p| p.string())
        .collect::<Vec<_>>()
        .join(", "),
      self.body.string()
    )
  }
}

#[derive(Clone, Debug)]
pub enum Either<Left, Right> {
  Left(Left),
  Right(Right),
}

impl<Left, Right> Node for Either<Left, Right>
where
  Left: Node,
  Right: Node,
{
  fn token_literal(&self) -> String {
    match self {
      Either::Left(node) => node.token_literal(),
      Either::Right(node) => node.token_literal(),
    }
  }

  fn string(&self) -> String {
    match self {
      Either::Left(node) => node.string(),
      Either::Right(node) => node.string(),
    }
  }
}

#[derive(Clone, Debug)]
pub struct CallExpression {
  pub token: Token,
  pub function: Either<Identifier, FunctionLiteral>,
  pub arguments: Vec<Expr>,
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
  Call(CallExpression),
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
      Expr::Function(fn_literal) => fn_literal.token_literal(),
      Expr::Call(call_expr) => call_expr.token.literal(),
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
      Expr::Function(fn_literal) => fn_literal.string(),
      Expr::Call(call_expr) => {
        format!(
          "{}({})",
          call_expr.function.string(),
          call_expr
            .arguments
            .iter()
            .map(|a| a.string())
            .collect::<Vec<_>>()
            .join(", ")
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
