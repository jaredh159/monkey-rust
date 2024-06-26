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
        .map(Node::string)
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
        .map(Node::string)
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
        .map(Node::string)
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
      Self::Left(node) => node.token_literal(),
      Self::Right(node) => node.token_literal(),
    }
  }

  fn string(&self) -> String {
    match self {
      Self::Left(node) => node.string(),
      Self::Right(node) => node.string(),
    }
  }
}

impl Node for Expr {
  fn token_literal(&self) -> String {
    match self {
      Self::Ident(ident) => ident.token.literal(),
      Self::Hash(hash_lit) => hash_lit.token.literal(),
      Self::Int(int) => int.token_literal(),
      Self::Prefix(prefix) => prefix.token_literal(),
      Self::Infix(infix) => infix.token_literal(),
      Self::Bool(bool_lit) => bool_lit.token_literal(),
      Self::If(if_expr) => if_expr.token.literal(),
      Self::Func(fn_literal) => fn_literal.token_literal(),
      Self::Call(call_expr) => call_expr.token_literal(),
      Self::String(string) => string.token_literal(),
      Self::Array(array) => array.token_literal(),
      Self::Index(index) => index.token_literal(),
    }
  }

  fn string(&self) -> String {
    match self {
      Self::Ident(ident) => ident.string(),
      Self::Hash(hash_lit) => hash_lit.string(),
      Self::Int(int) => int.string(),
      Self::Bool(bool_lit) => bool_lit.string(),
      Self::Prefix(prefix) => prefix.string(),
      Self::Infix(infix) => infix.string(),
      Self::Func(fn_literal) => fn_literal.string(),
      Self::Call(call_expr) => call_expr.string(),
      Self::If(if_expr) => if_expr.string(),
      Self::String(string) => string.string(),
      Self::Array(array) => array.string(),
      Self::Index(index) => index.string(),
    }
  }
}
