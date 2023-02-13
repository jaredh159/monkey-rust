use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::mem;

type PrefixParseFn = fn(&mut Parser) -> Option<Expr>;
type InfixParseFn = fn(&mut Parser, Expr) -> Option<Expr>;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
  Lowest,
  Equals,
  LessGreater,
  Sum,
  Product,
  Prefix,
  // Call,
}

impl Token {
  fn precedence(&self) -> Precedence {
    match self {
      Token::Eq => Precedence::Equals,
      Token::NotEq => Precedence::Equals,
      Token::Lt => Precedence::LessGreater,
      Token::Gt => Precedence::LessGreater,
      Token::Plus => Precedence::Sum,
      Token::Minus => Precedence::Sum,
      Token::Slash => Precedence::Product,
      Token::Asterisk => Precedence::Product,
      _ => Precedence::Lowest,
    }
  }
}

pub struct Parser {
  lexer: Lexer,
  cur_token: Token,
  peek_token: Token,
  errors: Vec<ParsingError>,
}

pub enum ParsingError {
  UnexpectedToken(String),
  IntegerConversionError(String),
  NoPrefixParseFn(Token),
}

#[cfg(test)]
impl ParsingError {
  fn message(&self) -> String {
    match self {
      ParsingError::UnexpectedToken(message) => format!("unexpected token - {}", message),
      ParsingError::IntegerConversionError(string) => format!("could not parse {} to i64", string),
      ParsingError::NoPrefixParseFn(token) => {
        format!("no prefix parse fn for {}", token.type_string())
      }
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

  fn prefix_parse_fn(&mut self) -> Option<PrefixParseFn> {
    match &self.cur_token {
      Token::Ident(_) => Some(Parser::parse_identifier),
      Token::Int(_) => Some(Parser::parse_integer_literal),
      Token::Bang | Token::Minus => Some(Parser::parse_prefix_expression),
      Token::True | Token::False => Some(Parser::parse_boolean_literal),
      Token::LParen => Some(Parser::parse_grouped_expression),
      _ => {
        self
          .errors
          .push(ParsingError::NoPrefixParseFn(self.cur_token.clone()));
        None
      }
    }
  }

  fn infix_parse_fn(&mut self) -> Option<InfixParseFn> {
    match &self.peek_token {
      Token::Eq
      | Token::NotEq
      | Token::Lt
      | Token::Gt
      | Token::Plus
      | Token::Minus
      | Token::Slash
      | Token::Asterisk => Some(Parser::parse_infix_expression),
      _ => None,
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

  fn parse_expression(&mut self, precedence: Precedence) -> Option<Expr> {
    let expr = self
      .prefix_parse_fn()
      .map(|prefix_fn| prefix_fn(self))
      .flatten();

    let mut expr = match expr {
      Some(expr) => expr,
      None => return None,
    };

    while self.peek_token != Token::Semicolon && precedence < self.peek_token.precedence() {
      if let Some(infix_fn) = self.infix_parse_fn() {
        self.advance();
        expr = infix_fn(self, expr.clone()).unwrap_or(expr);
      } else {
        return Some(expr);
      }
    }
    Some(expr)
  }

  fn parse_prefix_expression(&mut self) -> Option<Expr> {
    let initial_token = self.cur_token.clone();
    self.advance();
    self.parse_expression(Precedence::Prefix).map(|expr| {
      Expr::Prefix(
        initial_token.clone(),
        initial_token.literal(),
        Box::new(expr),
      )
    })
  }

  fn parse_infix_expression(&mut self, left: Expr) -> Option<Expr> {
    let operator_token = self.cur_token.clone();
    let precedence = operator_token.precedence();
    self.advance();
    self.parse_expression(precedence).map(|right| {
      Expr::Infix(
        operator_token.clone(),
        Box::new(left),
        operator_token.literal(),
        Box::new(right),
      )
    })
  }

  fn parse_identifier(&mut self) -> Option<Expr> {
    Some(Expr::Ident(Identifier {
      token: self.cur_token.clone(),
      value: self.cur_token.literal(),
    }))
  }

  fn parse_boolean_literal(&mut self) -> Option<Expr> {
    Some(Expr::BooleanLiteral(
      self.cur_token.clone(),
      self.cur_token == Token::True,
    ))
  }

  fn parse_integer_literal(&mut self) -> Option<Expr> {
    self
      .cur_token
      .literal()
      .parse::<i64>()
      .ok()
      .map(|value| Expr::IntegerLiteral(self.cur_token.clone(), value))
      .or_else(|| {
        self.errors.push(ParsingError::IntegerConversionError(
          self.cur_token.literal(),
        ));
        None
      })
  }

  fn parse_grouped_expression(&mut self) -> Option<Expr> {
    self.advance();
    let exp = self.parse_expression(Precedence::Lowest);
    if !self.advance_expecting(Token::RParen) {
      return None;
    }
    exp
  }

  fn parse_statement(&mut self) -> Option<Statement> {
    return match self.cur_token {
      Token::Let => self.parse_let_statement(),
      Token::Return => self.parse_return_statement(),
      _ => self.parse_expression_statement(),
    };
  }

  fn parse_expression_statement(&mut self) -> Option<Statement> {
    let initial_token = self.cur_token.clone();
    let expression = self.parse_expression(Precedence::Lowest);
    if expression.is_none() {
      return None;
    }
    if self.peek_token == Token::Semicolon {
      self.advance();
    }
    return Some(Statement::Expression(initial_token, expression.unwrap()));
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

  enum Lit<'a> {
    Int(i64),
    Str(&'a str),
    Bool(bool),
  }

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

  #[test]
  fn test_identifier_expressions() {
    let program = assert_program("foobar;", 1);
    let expr = assert_expression_statement(&program[0]);
    assert_identifier(&expr, "foobar");
  }

  #[test]
  fn test_integer_literal_expression() {
    let program = assert_program("5;", 1);
    let expr = assert_expression_statement(&program[0]);
    assert_integer_literal(expr, 5);
  }

  #[test]
  fn test_boolean_expressions() {
    let cases = vec![("true;", true), ("false;", false)];
    for (input, expected) in cases {
      let program = assert_program(input, 1);
      let expr = assert_expression_statement(&program[0]);
      assert_boolean_literal(expr, expected);
    }
  }

  #[test]
  fn test_parsing_prefix_expressions() {
    let cases = vec![
      ("!true;", "!", Lit::Bool(true)),
      ("!false;", "!", Lit::Bool(false)),
      ("!5;", "!", Lit::Int(5)),
      ("!5;", "!", Lit::Int(5)),
      ("-15;", "-", Lit::Int(15)),
    ];
    for (input, expected_operator, expected_value) in cases {
      let program = assert_program(input, 1);
      let expr = assert_expression_statement(&program[0]);
      if let Expr::Prefix(_, operator, expr) = expr {
        assert_eq!(operator, expected_operator);
        assert_literal(expr, expected_value);
      } else {
        assert!(false, "expression not a prefix, got {:?}", expr);
      }
    }
  }

  #[test]
  fn test_parsing_infix_expressions() {
    let cases = vec![
      ("true != false;", Lit::Bool(true), "!=", Lit::Bool(false)),
      ("false == false;", Lit::Bool(false), "==", Lit::Bool(false)),
      ("true == true;", Lit::Bool(true), "==", Lit::Bool(true)),
      ("5 + 5;", Lit::Int(5), "+", Lit::Int(5)),
      ("5 - 5;", Lit::Int(5), "-", Lit::Int(5)),
      ("5 * 5;", Lit::Int(5), "*", Lit::Int(5)),
      ("5 / 5;", Lit::Int(5), "/", Lit::Int(5)),
      ("5 > 5;", Lit::Int(5), ">", Lit::Int(5)),
      ("5 < 5;", Lit::Int(5), "<", Lit::Int(5)),
      ("5 == 5;", Lit::Int(5), "==", Lit::Int(5)),
      ("5 != 5;", Lit::Int(5), "!=", Lit::Int(5)),
      ("foo != bar;", Lit::Str("foo"), "!=", Lit::Str("bar")),
    ];
    for (input, lhs, operator, rhs) in cases {
      let program = assert_program(input, 1);
      let expr = assert_expression_statement(&program[0]);
      assert_infix(&expr, lhs, operator, rhs);
    }
  }

  #[test]
  fn test_operator_precedence() {
    let cases = vec![
      ("true", "true"),
      ("false", "false"),
      ("3 > 5 == false", "((3 > 5) == false)"),
      ("3 < 5 == true", "((3 < 5) == true)"),
      ("-a * b", "((-a) * b)"),
      ("!-a", "(!(-a))"),
      ("a + b + c", "((a + b) + c)"),
      ("a + b - c", "((a + b) - c)"),
      ("a * b * c", "((a * b) * c)"),
      ("a * b / c", "((a * b) / c)"),
      ("a + b / c", "(a + (b / c))"),
      ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
      ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
      ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
      ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
      ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
      ("(5 + 5) * 2", "((5 + 5) * 2)"),
      ("2 / (5 + 5)", "(2 / (5 + 5))"),
      ("-(5 + 5)", "(-(5 + 5))"),
      ("!(true == true)", "(!(true == true))"),
      (
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
      ),
    ];
    for (input, expected) in cases {
      let lexer = Lexer::from(input);
      let mut parser = Parser::new(lexer);
      let program = parser.parse_program();
      assert_no_parser_errors(&parser);
      assert_eq!(program.string(), expected)
    }
  }

  fn assert_literal<'a>(expr: &Expr, lit: Lit<'a>) {
    match lit {
      Lit::Int(int) => {
        assert_integer_literal(expr, int);
      }
      Lit::Str(string) => {
        assert_identifier(expr, string);
      }
      Lit::Bool(boolean) => {
        assert_boolean_literal(expr, boolean);
      }
    }
  }

  fn assert_identifier<'a>(expr: &'a Expr, expected_value: &str) -> &'a Identifier {
    if let Expr::Ident(ident) = expr {
      assert_eq!(expected_value, ident.token.literal());
      assert_eq!(expected_value, ident.value);
      return ident;
    } else {
      panic!("expression not an identifier, got {:?}", expr);
    }
  }

  fn assert_boolean_literal(expr: &Expr, expected_value: bool) {
    if let Expr::BooleanLiteral(token, value) = expr {
      assert_eq!(format!("{}", expected_value), token.literal());
      assert_eq!(*value, expected_value);
    } else {
      panic!("expression not a boolean literal, got {:?}", expr);
    }
  }

  fn assert_integer_literal(expr: &Expr, expected_value: i64) {
    if let Expr::IntegerLiteral(token, value) = expr {
      assert_eq!(expected_value.to_string(), token.literal());
      assert_eq!(expected_value, *value);
    } else {
      assert!(false, "expression not an integer literal, got {:?}", expr);
    }
  }

  fn assert_infix<'a>(
    expr: &Expr,
    expected_lhs: Lit<'a>,
    expected_operator: &str,
    expected_rhs: Lit<'a>,
  ) {
    if let Expr::Infix(_, lhs, operator, rhs) = expr {
      assert_literal(lhs, expected_lhs);
      assert_eq!(operator, expected_operator);
      assert_literal(rhs, expected_rhs);
    } else {
      assert!(false, "expression not a infix, got {:?}", expr);
    }
  }

  fn assert_expression_statement(statement: &Statement) -> &Expr {
    if let Statement::Expression(_, expr) = statement {
      return expr;
    } else {
      panic!("statement is not an Expression, got: {:?}", statement);
    }
  }

  fn assert_program(input: &str, num_expected_statements: usize) -> Program {
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    assert_no_parser_errors(&parser);
    assert_eq!(program.len(), num_expected_statements);
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
