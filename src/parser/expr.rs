use crate::parser::node::{Node, TokenNode};
use crate::parser::stmt::BlockStatement;
use crate::token::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
  Array(ArrayLiteral),
  Bool(BooleanLiteral),
  Call(CallExpression),
  Func(FunctionLiteral),
  Hash(HashLiteral),
  Ident(Identifier),
  If(IfExpression),
  Index(IndexExpression),
  Infix(InfixExpression),
  Int(IntegerLiteral),
  Prefix(PrefixExpression),
  String(StringLiteral),
}

// HashLiteral

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HashLiteral {
  pub token: Token,
  pub pairs: Vec<(Expr, Expr)>,
}

impl Node for HashLiteral {
  fn token_literal(&self) -> String {
    self.token.literal()
  }

  fn string(&self) -> String {
    format!(
      "{{{}}}",
      self
        .pairs
        .iter()
        .map(|(key, val)| format!("{}:{}", key.string(), val.string()))
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

// IndexExpression

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexExpression {
  pub token: Token,
  pub left: Box<Expr>,
  pub index: Box<Expr>,
}

impl Node for IndexExpression {
  fn token_literal(&self) -> String {
    self.token.literal()
  }

  fn string(&self) -> String {
    format!("({}[{}])", self.left.string(), self.index.string())
  }
}

// ArrayLiteral

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayLiteral {
  pub token: Token,
  pub elements: Vec<Expr>,
}

impl Node for ArrayLiteral {
  fn token_literal(&self) -> String {
    self.token.literal()
  }

  fn string(&self) -> String {
    format!(
      "[{}]",
      self
        .elements
        .iter()
        .map(|e| e.string())
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

// StringLiteral

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringLiteral {
  pub token: Token,
  pub value: String,
}

impl TokenNode for StringLiteral {
  fn token(&self) -> &Token {
    &self.token
  }
}

// Bool

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BooleanLiteral {
  pub token: Token,
  pub value: bool,
}

impl TokenNode for BooleanLiteral {
  fn token(&self) -> &Token {
    &self.token
  }
}

// CallExpression

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallExpression {
  pub token: Token,
  pub function: Either<Identifier, FunctionLiteral>,
  pub arguments: Vec<Expr>,
}

impl Node for CallExpression {
  fn token_literal(&self) -> String {
    self.token.literal()
  }
  fn string(&self) -> String {
    format!(
      "{}({})",
      self.function.string(),
      self
        .arguments
        .iter()
        .map(|a| a.string())
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

// Identifier

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Identifier {
  pub token: Token,
  pub value: String,
}

impl TokenNode for Identifier {
  fn token(&self) -> &Token {
    &self.token
  }
}

// IfExpression

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfExpression {
  pub token: Token,
  pub condition: Box<Expr>,
  pub consequence: BlockStatement,
  pub alternative: Option<BlockStatement>,
}

impl Node for IfExpression {
  fn token_literal(&self) -> String {
    self.token.literal()
  }
  fn string(&self) -> String {
    let mut string = format!(
      "if {} {}",
      self.condition.string(),
      self.consequence.string()
    );
    if let Some(alternative) = &self.alternative {
      string.push_str(&format!("else {}", alternative.string()));
    }
    string
  }
}

// InfixExpression

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InfixExpression {
  pub token: Token,
  pub lhs: Box<Expr>,
  pub operator: String,
  pub rhs: Box<Expr>,
}

impl Node for InfixExpression {
  fn token_literal(&self) -> String {
    self.token.literal()
  }
  fn string(&self) -> String {
    format!(
      "({} {} {})",
      self.lhs.string(),
      self.operator,
      self.rhs.string()
    )
  }
}

// IntegerLiteral

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntegerLiteral {
  pub token: Token,
  pub value: i64,
}

impl TokenNode for IntegerLiteral {
  fn token(&self) -> &Token {
    &self.token
  }
}

// FunctionLiteral

#[derive(Clone, Debug, PartialEq, Eq)]
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

// PrefixExpression

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrefixExpression {
  pub token: Token,
  pub operator: String,
  pub rhs: Box<Expr>,
}

impl Node for PrefixExpression {
  fn token_literal(&self) -> String {
    self.token.literal()
  }
  fn string(&self) -> String {
    format!("({}{})", self.operator, self.rhs.string())
  }
}

// Either

#[derive(Clone, Debug, PartialEq, Eq)]
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

impl Node for Expr {
  fn token_literal(&self) -> String {
    match self {
      Expr::Ident(ident) => ident.token.literal(),
      Expr::Hash(hash_lit) => hash_lit.token.literal(),
      Expr::Int(int) => int.token_literal(),
      Expr::Prefix(prefix) => prefix.token_literal(),
      Expr::Infix(infix) => infix.token_literal(),
      Expr::Bool(bool_lit) => bool_lit.token_literal(),
      Expr::If(if_expr) => if_expr.token.literal(),
      Expr::Func(fn_literal) => fn_literal.token_literal(),
      Expr::Call(call_expr) => call_expr.token_literal(),
      Expr::String(string) => string.token_literal(),
      Expr::Array(array) => array.token_literal(),
      Expr::Index(index) => index.token_literal(),
    }
  }

  fn string(&self) -> String {
    match self {
      Expr::Ident(ident) => ident.string(),
      Expr::Hash(hash_lit) => hash_lit.string(),
      Expr::Int(int) => int.string(),
      Expr::Bool(bool_lit) => bool_lit.string(),
      Expr::Prefix(prefix) => prefix.string(),
      Expr::Infix(infix) => infix.string(),
      Expr::Func(fn_literal) => fn_literal.string(),
      Expr::Call(call_expr) => call_expr.string(),
      Expr::If(if_expr) => if_expr.string(),
      Expr::String(string) => string.string(),
      Expr::Array(array) => array.string(),
      Expr::Index(index) => index.string(),
    }
  }
}
