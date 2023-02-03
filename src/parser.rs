use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

pub struct Parser {
  lexer: Lexer,
  cur_token: Token,
  peek_token: Token,
}

impl Parser {
  pub fn new(mut lexer: Lexer) -> Parser {
    let cur_token = lexer.next().unwrap();
    let peek_token = lexer.next().unwrap();
    Parser {
      lexer,
      cur_token,
      peek_token,
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
      _ => None,
    };
  }

  fn parse_let_statement(&mut self) -> Option<Statement> {
    let let_token = self.cur_token.clone();

    if !self.advance_if_peek(Token::Ident(String::new())) {
      return None;
    }

    let name = Identifier {
      token: self.cur_token.clone(),
      value: self.cur_token.literal(),
    };

    if !self.advance_if_peek(Token::Assign) {
      return None;
    }

    // TODO: skip over expression for now
    while !self.cur_token.same_variant(Token::Semicolon) {
      self.advance();
    }

    Some(Statement::Let(let_token, name, Expr::Todo))
  }

  fn advance_if_peek(&mut self, token: Token) -> bool {
    if self.peek_token.same_variant(token) {
      self.advance();
      return true;
    }
    return false;
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

    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    assert_eq!(program.len(), 3);

    let cases = vec!["x", "y", "foobar"];
    for (statement, expected) in program.iter().zip(cases.iter()) {
      test_let_statement(statement, expected);
    }
  }

  fn test_let_statement(statement: &Statement, expected_ident: &str) {
    if let Statement::Let(_, ident, _) = statement {
      assert_eq!(ident.value, expected_ident);
      assert_eq!(ident.token_literal(), expected_ident);
    } else {
      assert!(false, "expected let statement, got {:?}", statement);
    }
  }
}
