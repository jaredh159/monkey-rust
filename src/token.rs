use enum_as_inner::EnumAsInner;
use std::mem;

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum Token {
  Illegal,
  EOF,
  Ident(String),
  Int(String),
  Comma,
  Semicolon,
  LParen,
  RParen,
  LBrace,
  RBrace,

  // keywords
  Function,
  Let,
  True,
  False,
  If,
  Else,
  Return,

  // operators
  Assign,
  Plus,
  Minus,
  Bang,
  Asterisk,
  Slash,

  Lt,
  Gt,
  Eq,
  NotEq,
}

impl Token {
  pub fn same_variant(&self, other: &Token) -> bool {
    match self {
      Token::Int(_) => mem::discriminant(other) == mem::discriminant(&self),
      Token::Ident(_) => mem::discriminant(other) == mem::discriminant(&self),
      _ => self == other,
    }
  }

  pub fn literal(&self) -> String {
    match self {
      Token::Illegal => "".to_string(),
      Token::EOF => "".to_string(),
      Token::Ident(ident) => ident.clone(),
      Token::Int(int) => int.clone(),
      Token::Comma => ",".to_string(),
      Token::Semicolon => ";".to_string(),
      Token::LParen => "(".to_string(),
      Token::RParen => ")".to_string(),
      Token::LBrace => "{".to_string(),
      Token::RBrace => "}".to_string(),
      Token::Function => "fn".to_string(),
      Token::Let => "let".to_string(),
      Token::True => "true".to_string(),
      Token::False => "false".to_string(),
      Token::If => "if".to_string(),
      Token::Else => "else".to_string(),
      Token::Return => "return".to_string(),
      Token::Assign => "=".to_string(),
      Token::Plus => "+".to_string(),
      Token::Minus => "-".to_string(),
      Token::Bang => "+".to_string(),
      Token::Asterisk => "*".to_string(),
      Token::Slash => "/".to_string(),
      Token::Lt => "<".to_string(),
      Token::Gt => ">".to_string(),
      Token::Eq => "==".to_string(),
      Token::NotEq => "!=".to_string(),
    }
  }

  pub fn type_string(&self) -> &'static str {
    match self {
      Token::Illegal => "Token::Illegal",
      Token::EOF => "Token::EOF",
      Token::Ident(_) => "Token::Ident",
      Token::Int(_) => "Token::Int",
      Token::Comma => "Token::Comma",
      Token::Semicolon => "Token::Semicolon",
      Token::LParen => "Token::LParen",
      Token::RParen => "Token::RParen",
      Token::LBrace => "Token::LBrace",
      Token::RBrace => "Token::RBrace",
      Token::Function => "Token::Function",
      Token::Let => "Token::Let",
      Token::True => "Token::True",
      Token::False => "Token::False",
      Token::If => "Token::If",
      Token::Else => "Token::Else",
      Token::Return => "Token::Return",
      Token::Assign => "Token::Assign",
      Token::Plus => "Token::Plus",
      Token::Minus => "Token::Minus",
      Token::Bang => "Token::Bang",
      Token::Asterisk => "Token::Asterisk",
      Token::Slash => "Token::Slash",
      Token::Lt => "Token::Lt",
      Token::Gt => "Token::Gt",
      Token::Eq => "Token::Eq",
      Token::NotEq => "Token::NotEq",
    }
  }
}
