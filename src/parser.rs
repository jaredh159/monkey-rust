use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

pub struct Parser {
  lexer: Lexer,
  cur_token: Token,
  peek_token: Token,
  errors: Vec<ParsingError>,
}

pub enum ParsingError {
  UnexpectedToken(String),
}

impl ParsingError {
  fn message(&self) -> String {
    match self {
      ParsingError::UnexpectedToken(message) => format!("unexpected token - {}", message),
    }
  }
}

impl Parser {
  pub fn new(mut lexer: Lexer) -> Parser {
    let cur_token = lexer.next().unwrap();
    let peek_token = lexer.next().unwrap();
    Parser {
      lexer,
      cur_token,
      peek_token,
      errors: Vec::new(),
    }
  }

  pub fn parse_program(&mut self) -> Program {
    let mut program = Vec::new();
    while self.cur_token != Token::EOF {
      if let Some(statement) = self.parse_statement() {
        program.push(statement);
      }
      self.advance();
    }
    return program;
  }

  fn parse_statement(&mut self) -> Option<Statement> {
    return match self.cur_token {
      Token::Let => self.parse_let_statement(),
      Token::Return => self.parse_return_statement(),
      _ => None,
    };
  }

  fn parse_return_statement(&mut self) -> Option<Statement> {
    let return_token = self.cur_token.clone();
    self.advance();

    // TODO: skip over expression for now
    while !self.cur_token.same_variant(&Token::Semicolon) {
      self.advance();
    }

    Some(Statement::Return(return_token, Expr::Todo))
  }

  fn parse_let_statement(&mut self) -> Option<Statement> {
    let let_token = self.cur_token.clone();

    if !self.advance_expecting(Token::Ident(String::new())) {
      return None;
    }

    let name = Identifier {
      token: self.cur_token.clone(),
      value: self.cur_token.literal(),
    };

    if !self.advance_expecting(Token::Assign) {
      return None;
    }

    // TODO: skip over expression for now
    while !self.cur_token.same_variant(&Token::Semicolon) {
      self.advance();
    }

    Some(Statement::Let(let_token, name, Expr::Todo))
  }

  fn advance_expecting(&mut self, token: Token) -> bool {
    if self.peek_token.same_variant(&token) {
      self.advance();
      return true;
    } else {
      self.errors.push(ParsingError::UnexpectedToken(format!(
        "expected next token to be `{}`, got `{}` instead",
        token.type_string(),
        self.peek_token.type_string()
      )));
      return false;
    }
  }

  fn advance(&mut self) {
    let next_peek = self.lexer.next_token();
    let prev_peek = mem::replace(&mut self.peek_token, next_peek);
    self.cur_token = prev_peek;
  }
}

// tests

#[cfg(test)]
mod tests {
  use crate::ast::*;
  use crate::lexer::*;
  use crate::parser::*;

  #[test]
  fn test_let_statements() {
    let input = r#"
    let x = 5;
    let y = 10;
    let foobar = 838383;
    "#;

    let program = assert_program(input, 3);

    let cases = vec!["x", "y", "foobar"];
    for (statement, expected) in program.iter().zip(cases.iter()) {
      test_let_statement(statement, expected);
    }
  }

  #[test]
  fn test_return_statements() {
    let input = r#"
    return 5;
    return 10;
    return 993322;
    "#;

    let program = assert_program(input, 3);
    for statement in program {
      if let Statement::Return(token, _) = statement {
        assert_eq!(token.literal(), "return");
      } else {
        assert!(false, "expected return statement, got {:?}", statement);
      }
    }
  }

  fn assert_program(input: &str, num_expected_statements: usize) -> Program {
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    assert_no_parser_errors(&parser);
    assert_eq!(program.len(), 3);
    program
  }

  fn test_let_statement(statement: &Statement, expected_ident: &str) {
    if let Statement::Let(_, ident, _) = statement {
      assert_eq!(ident.value, expected_ident);
      assert_eq!(ident.token_literal(), expected_ident);
    } else {
      assert!(false, "expected let statement, got {:?}", statement);
    }
  }

  fn assert_no_parser_errors(parser: &Parser) {
    for error in &parser.errors {
      eprintln!("parser error: {}", error.message());
    }
    assert_eq!(parser.errors.len(), 0);
  }
}
