use crate::token::Token;

pub enum ParsingError {
  UnexpectedToken(String),
  IntegerConversionError(String),
  NoPrefixParseFn(Token),
}

impl ParsingError {
  pub fn message(&self) -> String {
    match self {
      Self::UnexpectedToken(message) => format!("unexpected token - {message}"),
      Self::IntegerConversionError(string) => {
        format!("could not parse {string} to i64")
      }
      Self::NoPrefixParseFn(token) => {
        format!("no prefix parse fn for {}", token.type_string())
      }
    }
  }
}
