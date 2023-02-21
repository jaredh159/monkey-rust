use crate::parser::{err::ParsingError, node::Program, stmt::Statement};
use crate::token::Token;
use crate::{lexer::Lexer, parser::expr::*};
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
  Call,
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
      Token::LParen => Precedence::Call,
      _ => Precedence::Lowest,
    }
  }
}

pub struct Parser {
  lexer: Lexer,
  cur_token: Token,
  peek_token: Token,
  pub errors: Vec<ParsingError>,
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
      Token::If => Some(Parser::parse_if_expression),
      Token::Function => Some(Parser::parse_fn_literal),
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
      Token::LParen => Some(Parser::parse_call_expression),
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
      Expr::Prefix(PrefixExpression {
        token: initial_token.clone(),
        operator: initial_token.literal(),
        rhs: Box::new(expr),
      })
    })
  }

  fn parse_infix_expression(&mut self, left: Expr) -> Option<Expr> {
    let operator_token = self.cur_token.clone();
    let precedence = operator_token.precedence();
    self.advance();
    self.parse_expression(precedence).map(|right| {
      Expr::Infix(InfixExpression {
        token: operator_token.clone(),
        lhs: Box::new(left),
        operator: operator_token.literal(),
        rhs: Box::new(right),
      })
    })
  }

  fn parse_block_statement(&mut self) -> BlockStatement {
    let initial_token = self.cur_token.clone();
    let mut statements: Vec<Statement> = Vec::new();
    self.advance();

    while self.cur_token != Token::RBrace && self.cur_token != Token::EOF {
      if let Some(stmt) = self.parse_statement() {
        statements.push(stmt);
      }
      self.advance();
    }

    BlockStatement {
      token: initial_token,
      statements,
    }
  }

  fn parse_if_expression(&mut self) -> Option<Expr> {
    let initial_token = self.cur_token.clone();
    if !self.advance_expecting(Token::LParen) {
      return None;
    }

    self.advance();
    let condition = match self.parse_expression(Precedence::Lowest) {
      Some(cond) => cond,
      None => return None,
    };

    if !self.advance_expecting(Token::RParen) {
      return None;
    }

    if !self.advance_expecting(Token::LBrace) {
      return None;
    }

    let consequence = self.parse_block_statement();
    let mut alternative: Option<BlockStatement> = None;

    if self.peek_token == Token::Else {
      self.advance();
      if !self.advance_expecting(Token::LBrace) {
        return None;
      }
      alternative = Some(self.parse_block_statement());
    }

    Some(Expr::If(IfExpression {
      token: initial_token,
      condition: Box::new(condition),
      consequence,
      alternative,
    }))
  }

  fn parse_identifier(&mut self) -> Option<Expr> {
    Some(Expr::Ident(Identifier {
      token: self.cur_token.clone(),
      value: self.cur_token.literal(),
    }))
  }

  fn parse_boolean_literal(&mut self) -> Option<Expr> {
    Some(Expr::Bool(BooleanLiteral {
      token: self.cur_token.clone(),
      value: self.cur_token == Token::True,
    }))
  }

  fn parse_integer_literal(&mut self) -> Option<Expr> {
    self
      .cur_token
      .literal()
      .parse::<i64>()
      .ok()
      .map(|value| {
        Expr::Int(IntegerLiteral {
          token: self.cur_token.clone(),
          value,
        })
      })
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

  fn parse_fn_literal(&mut self) -> Option<Expr> {
    let token = self.cur_token.clone();
    if !self.advance_expecting(Token::LParen) {
      return None;
    }

    let parameters = match self.parse_fn_params() {
      Some(params) => params,
      None => return None,
    };

    if !self.advance_expecting(Token::LBrace) {
      return None;
    }

    Some(Expr::Func(FunctionLiteral {
      token,
      parameters,
      body: self.parse_block_statement(),
    }))
  }

  fn parse_call_expression(&mut self, left: Expr) -> Option<Expr> {
    let token = self.cur_token.clone();
    let arguments = match self.parse_call_arguments() {
      Some(expr) => expr,
      None => return None,
    };

    let function = match left {
      Expr::Ident(ident) => Either::Left(ident),
      Expr::Func(fn_lit) => Either::Right(fn_lit),
      _ => return None,
    };

    Some(Expr::Call(CallExpression {
      token,
      function,
      arguments,
    }))
  }

  fn parse_call_arguments(&mut self) -> Option<Vec<Expr>> {
    let mut args = Vec::new();
    if self.peek_token == Token::RParen {
      self.advance();
      return Some(args);
    }

    self.advance();
    let first_arg = match self.parse_expression(Precedence::Lowest) {
      Some(expr) => expr,
      None => return None,
    };
    args.push(first_arg);

    while self.peek_token == Token::Comma {
      self.advance();
      self.advance();
      let arg = match self.parse_expression(Precedence::Lowest) {
        Some(expr) => expr,
        None => return None,
      };
      args.push(arg)
    }

    if !self.advance_expecting(Token::RParen) {
      return None;
    }

    Some(args)
  }

  fn parse_fn_params(&mut self) -> Option<Vec<Identifier>> {
    let mut identifiers = Vec::new();
    if self.peek_token == Token::RParen {
      self.advance();
      return Some(identifiers);
    }

    self.advance();

    // first param
    identifiers.push(Identifier {
      token: self.cur_token.clone(),
      value: self.cur_token.literal(),
    });

    while self.peek_token == Token::Comma {
      self.advance();
      self.advance();
      identifiers.push(Identifier {
        token: self.cur_token.clone(),
        value: self.cur_token.literal(),
      });
    }

    if !self.advance_expecting(Token::RParen) {
      return None;
    }

    Some(identifiers)
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

    let return_value = match self.parse_expression(Precedence::Lowest) {
      Some(expr) => expr,
      None => return None,
    };

    if self.peek_token == Token::Semicolon {
      self.advance();
    }

    Some(Statement::Return(return_token, return_value))
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

    self.advance();
    let value = match self.parse_expression(Precedence::Lowest) {
      Some(expr) => expr,
      None => return None,
    };

    if self.peek_token == Token::Semicolon {
      self.advance();
    }

    Some(Statement::Let(let_token, name, value))
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
  use crate::lexer::*;
  use crate::parser::expr::{Either, Expr, Identifier};
  use crate::parser::node::{Node, Program};
  use crate::parser::stmt::Statement;
  use crate::parser::Parser;

  enum Lit<'a> {
    Int(i64),
    Str(&'a str),
    Bool(bool),
  }

  #[test]
  fn test_let_statements() {
    let cases = vec![
      ("let x = 5;", "x", Lit::Int(5)),
      ("let y = true;", "y", Lit::Bool(true)),
      ("let foobar = y;", "foobar", Lit::Str("y")),
    ];

    for (input, expected_ident, expected_value) in cases {
      let program = assert_program(input, 1);
      let expr = assert_let_statement(&program[0], expected_ident);
      assert_literal(&expr, expected_value);
    }
  }

  #[test]
  fn test_return_statements() {
    let input = r#"
    return 5;
    return 10;
    return 993322;
    "#;

    let cases = vec![
      ("return 5;", Lit::Int(5)),
      ("return true;", Lit::Bool(true)),
      ("return foobar;", Lit::Str("foobar")),
    ];

    for (input, expected_expr) in cases {
      let program = assert_program(input, 1);
      if let Statement::Return(token, expr) = &program[0] {
        assert_eq!(token.literal(), "return");
        assert_literal(&expr, expected_expr);
      } else {
        panic!("expected return statement, got {:?}", program[0]);
      }
    }

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
      if let Expr::Prefix(prefix) = expr {
        assert_eq!(prefix.operator, expected_operator);
        assert_literal(&prefix.rhs, expected_value);
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
      ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
      (
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
      ),
      (
        "add(a + b + c * d / f + g)",
        "add((((a + b) + ((c * d) / f)) + g))",
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

  #[test]
  fn test_if_expression() {
    let program = assert_program("if (x < y) { x }", 1);
    let expr = assert_expression_statement(&program[0]);
    if let Expr::If(expr) = expr {
      assert_infix(&expr.condition, Lit::Str("x"), "<", Lit::Str("y"));
      assert_eq!(expr.consequence.statements.len(), 1);
      let consequence = assert_expression_statement(&expr.consequence.statements[0]);
      assert_identifier(consequence, "x");
      assert!(expr.alternative.is_none());
    } else {
      panic!("expression not a prefix, got {:?}", expr);
    }
  }

  #[test]
  fn test_if_else_expression() {
    let program = assert_program("if (x < y) { x } else { y }", 1);
    let expr = assert_expression_statement(&program[0]);
    if let Expr::If(expr) = expr {
      assert_infix(&expr.condition, Lit::Str("x"), "<", Lit::Str("y"));
      assert_eq!(expr.consequence.statements.len(), 1);
      let cons_stmt = assert_expression_statement(&expr.consequence.statements[0]);
      assert_identifier(cons_stmt, "x");
      let alternative = expr
        .alternative
        .as_ref()
        .expect("expected alternative not to be missing");
      assert_eq!(alternative.statements.len(), 1);
      let alternative_stmt = assert_expression_statement(&alternative.statements[0]);
      assert_identifier(alternative_stmt, "y");
    } else {
      panic!("expression not a prefix, got {:?}", expr);
    }
  }

  #[test]
  fn test_function_literal() {
    let program = assert_program("fn(x, y) { x + y ; }", 1);
    let expr = assert_expression_statement(&program[0]);
    if let Expr::Func(fn_lit) = expr {
      assert_eq!(fn_lit.parameters.len(), 2);
      assert_eq!(fn_lit.parameters[0].value, "x");
      assert_eq!(fn_lit.parameters[1].value, "y");
      assert_eq!(fn_lit.body.statements.len(), 1);
      let expr = assert_expression_statement(&fn_lit.body.statements[0]);
      assert_infix(expr, Lit::Str("x"), "+", Lit::Str("y"));
    } else {
      panic!("expression not a function literal, got {:?}", expr);
    }
  }

  #[test]
  fn test_parameter_parsing() {
    let cases = vec![
      ("fn() {};", vec![]),
      ("fn(x) {};", vec!["x"]),
      ("fn(x, y, z) {};", vec!["x", "y", "z"]),
    ];
    for (input, expected) in cases {
      let program = assert_program(input, 1);
      let expr = assert_expression_statement(&program[0]);
      if let Expr::Func(fn_lit) = expr {
        assert_eq!(fn_lit.parameters.len(), expected.len());
        for (param, expected_ident) in fn_lit.parameters.iter().zip(expected) {
          assert_eq!(param.value, expected_ident);
        }
      } else {
        panic!("expression not a function literal, got {:?}", expr);
      }
    }
  }

  #[test]
  fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let program = assert_program(input, 1);
    let expr = assert_expression_statement(&program[0]);
    if let Expr::Call(call_expr) = expr {
      match &call_expr.function {
        Either::Left(ident) => assert_identifier(&Expr::Ident(ident.clone()), "add"),
        Either::Right(_) => panic!("expected identifier, got fn literal"),
      };
      assert_eq!(call_expr.arguments.len(), 3);
      assert_literal(&call_expr.arguments[0], Lit::Int(1));
      assert_infix(&call_expr.arguments[1], Lit::Int(2), "*", Lit::Int(3));
      assert_infix(&call_expr.arguments[2], Lit::Int(4), "+", Lit::Int(5));
    } else {
      panic!("expression not a all expression, got {:?}", expr);
    }
  }

  #[test]
  fn test_argument_parsing() {
    let cases = vec![
      ("add();", vec![]),
      ("add(x);", vec!["x"]),
      ("add(x, y, z);", vec!["x", "y", "z"]),
    ];
    for (input, expected) in cases {
      let program = assert_program(input, 1);
      let expr = assert_expression_statement(&program[0]);
      if let Expr::Call(call_expr) = expr {
        assert_eq!(call_expr.arguments.len(), expected.len());
        for (arg, expected_ident) in call_expr.arguments.iter().zip(expected) {
          assert_identifier(arg, expected_ident);
        }
      } else {
        panic!("expression not a call expression, got {:?}", expr);
      }
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
    if let Expr::Bool(bool_lit) = expr {
      assert_eq!(format!("{}", expected_value), bool_lit.token.literal());
      assert_eq!(bool_lit.value, expected_value);
    } else {
      panic!("expression not a boolean literal, got {:?}", expr);
    }
  }

  fn assert_integer_literal(expr: &Expr, expected_value: i64) {
    if let Expr::Int(int) = expr {
      assert_eq!(expected_value.to_string(), int.token_literal());
      assert_eq!(expected_value, int.value);
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
    if let Expr::Infix(infix) = expr {
      assert_literal(&infix.lhs, expected_lhs);
      assert_eq!(infix.operator, expected_operator);
      assert_literal(&infix.rhs, expected_rhs);
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

  fn assert_let_statement(statement: &Statement, expected_ident: &str) -> Expr {
    if let Statement::Let(_, ident, expr) = statement {
      assert_eq!(ident.value, expected_ident);
      assert_eq!(ident.token_literal(), expected_ident);
      return expr.clone();
    } else {
      panic!("expected let statement, got {:?}", statement);
    }
  }

  fn assert_no_parser_errors(parser: &Parser) {
    for error in &parser.errors {
      eprintln!("parser error: {}", error.message());
    }
    assert_eq!(parser.errors.len(), 0);
  }
}
