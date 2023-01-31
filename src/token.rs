#[derive(Debug, PartialEq, Clone)]
pub enum Token {
  Illegal,
  EOF,
  Ident(String),
  Int(String),
  Assign,
  Plus,
  Comma,
  Semicolon,
  LParen,
  RParen,
  LBrace,
  RBrace,
  Function,
  Let,
}
