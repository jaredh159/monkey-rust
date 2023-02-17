use crate::token::Token;

pub enum ParsingError {
  UnexpectedToken(String),
  IntegerConversionError(String),
  NoPrefixParseFn(Token),
}

#[cfg(test)]
impl ParsingError {
  pub fn message(&self) -> String {
    match self {
      ParsingError::UnexpectedToken(message) => format!("unexpected token - {}", message),
      ParsingError::IntegerConversionError(string) => format!("could not parse {} to i64", string),
      ParsingError::NoPrefixParseFn(token) => {
        format!("no prefix parse fn for {}", token.type_string())
      }
    }
  }
}
