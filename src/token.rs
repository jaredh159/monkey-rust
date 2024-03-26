use enum_as_inner::EnumAsInner;
use std::mem;

#[derive(Debug, PartialEq, Eq, Clone, EnumAsInner, Hash)]
pub enum Token {
  Illegal,
  Eof,
  Ident(String),
  Int(String),
  String(String),
  Comma,
  Colon,
  Semicolon,
  LParen,
  RParen,
  LBrace,
  RBrace,
  LBracket,
  RBracket,

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
  pub fn same_variant(&self, other: &Self) -> bool {
    match self {
      Self::Int(_) | Self::Ident(_) => {
        mem::discriminant(other) == mem::discriminant(self)
      }
      _ => self == other,
    }
  }

  pub fn literal(&self) -> String {
    match self {
      Self::Illegal | Self::Eof => String::new(),
      Self::Ident(ident) => ident.clone(),
      Self::Int(int) => int.clone(),
      Self::String(string) => string.clone(),
      Self::Comma => ",".to_string(),
      Self::Colon => ":".to_string(),
      Self::Semicolon => ";".to_string(),
      Self::LParen => "(".to_string(),
      Self::RParen => ")".to_string(),
      Self::LBrace => "{".to_string(),
      Self::RBrace => "}".to_string(),
      Self::LBracket => "[".to_string(),
      Self::RBracket => "]".to_string(),
      Self::Function => "fn".to_string(),
      Self::Let => "let".to_string(),
      Self::True => "true".to_string(),
      Self::False => "false".to_string(),
      Self::If => "if".to_string(),
      Self::Else => "else".to_string(),
      Self::Return => "return".to_string(),
      Self::Assign => "=".to_string(),
      Self::Plus => "+".to_string(),
      Self::Minus => "-".to_string(),
      Self::Bang => "!".to_string(),
      Self::Asterisk => "*".to_string(),
      Self::Slash => "/".to_string(),
      Self::Lt => "<".to_string(),
      Self::Gt => ">".to_string(),
      Self::Eq => "==".to_string(),
      Self::NotEq => "!=".to_string(),
    }
  }

  pub const fn type_string(&self) -> &'static str {
    match self {
      Self::Illegal => "Self::Illegal",
      Self::Eof => "Token::EOF",
      Self::Ident(_) => "Token::Ident",
      Self::Int(_) => "Token::Int",
      Self::String(_) => "Token::String",
      Self::Comma => "Token::Comma",
      Self::Colon => "Token::Colon",
      Self::Semicolon => "Token::Semicolon",
      Self::LParen => "Token::LParen",
      Self::RParen => "Token::RParen",
      Self::LBrace => "Token::LBrace",
      Self::RBrace => "Token::RBrace",
      Self::LBracket => "Token::LBracket",
      Self::RBracket => "Token::RBracket",
      Self::Function => "Token::Function",
      Self::Let => "Token::Let",
      Self::True => "Token::True",
      Self::False => "Token::False",
      Self::If => "Token::If",
      Self::Else => "Token::Else",
      Self::Return => "Token::Return",
      Self::Assign => "Token::Assign",
      Self::Plus => "Token::Plus",
      Self::Minus => "Token::Minus",
      Self::Bang => "Token::Bang",
      Self::Asterisk => "Token::Asterisk",
      Self::Slash => "Token::Slash",
      Self::Lt => "Token::Lt",
      Self::Gt => "Token::Gt",
      Self::Eq => "Token::Eq",
      Self::NotEq => "Token::NotEq",
    }
  }
}
