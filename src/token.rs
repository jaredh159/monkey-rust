#[derive(Debug, PartialEq, Clone)]
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
