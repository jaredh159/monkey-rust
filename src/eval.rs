use crate::object::{Integer, Obj};
use crate::parser::{Expr, IfExpression, Program, Statement};

pub enum Node {
  Prog(Program),
  Stmt(Statement),
  Expr(Expr),
}

pub fn eval(node: Node) -> Obj {
  match node {
    Node::Prog(program) => eval_statements(program),
    Node::Stmt(Statement::Expression(_, expr)) => eval(Node::Expr(expr)),
    Node::Stmt(Statement::Let(_, _, _)) => todo!(),
    Node::Stmt(Statement::Return(_, _)) => todo!(),
    Node::Stmt(Statement::Block(block)) => eval_statements(block.statements),
    Node::Expr(Expr::Bool(boolean)) => Obj::bool(boolean.value),
    Node::Expr(Expr::Call(_)) => todo!(),
    Node::Expr(Expr::Func(_)) => todo!(),
    Node::Expr(Expr::Ident(_)) => todo!(),
    Node::Expr(Expr::If(if_expr)) => eval_if_expression(if_expr),
    Node::Expr(Expr::Int(int)) => Obj::int(int.value),
    Node::Expr(Expr::Prefix(p)) => eval_prefix_expr(&p.operator, eval(Node::Expr(*p.rhs))),
    Node::Expr(Expr::Infix(infix)) => eval_infix_expr(
      eval(Node::Expr(*infix.lhs)),
      infix.operator,
      eval(Node::Expr(*infix.rhs)),
    ),
  }
}

fn eval_infix_expr(lhs: Obj, operator: String, rhs: Obj) -> Obj {
  match (lhs, operator.as_ref(), rhs) {
    (Obj::Int(lhs), _, Obj::Int(rhs)) => eval_integer_infix_expression(lhs, operator, rhs),
    (Obj::Bool(lhs), "==", Obj::Bool(rhs)) => Obj::bool(lhs.value == rhs.value),
    (Obj::Bool(lhs), "!=", Obj::Bool(rhs)) => Obj::bool(lhs.value != rhs.value),
    _ => Obj::Null,
  }
}

fn eval_integer_infix_expression(lhs: Integer, operator: String, rhs: Integer) -> Obj {
  match operator.as_ref() {
    "+" => Obj::int(lhs.value + rhs.value),
    "-" => Obj::int(lhs.value - rhs.value),
    "*" => Obj::int(lhs.value * rhs.value),
    "/" => Obj::int(lhs.value / rhs.value),
    "<" => Obj::bool(lhs.value < rhs.value),
    ">" => Obj::bool(lhs.value > rhs.value),
    "==" => Obj::bool(lhs.value == rhs.value),
    "!=" => Obj::bool(lhs.value != rhs.value),
    _ => Obj::Null,
  }
}

fn eval_prefix_expr(operator: &str, rhs: Obj) -> Obj {
  match operator {
    "!" => eval_bang_operator_expression(rhs),
    "-" => eval_minus_prefix_operator_expression(rhs),
    _ => Obj::Null,
  }
}

fn eval_bang_operator_expression(rhs: Obj) -> Obj {
  match rhs {
    Obj::Bool(b) => Obj::bool(!b.value),
    Obj::Null => Obj::bool(true),
    _ => Obj::bool(false),
  }
}

fn eval_minus_prefix_operator_expression(rhs: Obj) -> Obj {
  match rhs {
    Obj::Int(int) => Obj::int(-int.value),
    _ => Obj::Null,
  }
}

fn eval_if_expression(if_expr: IfExpression) -> Obj {
  let condition = eval(Node::Expr(*if_expr.condition));
  if condition.is_truthy() {
    eval(Node::Stmt(Statement::Block(if_expr.consequence)))
  } else if if_expr.alternative.is_some() {
    eval(Node::Stmt(Statement::Block(if_expr.alternative.unwrap())))
  } else {
    Obj::Null
  }
}

fn eval_statements(stmts: Vec<Statement>) -> Obj {
  let mut result = Obj::Null;
  for stmt in stmts {
    result = eval(Node::Stmt(stmt));
  }
  result
}

// tests

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::Lexer;
  use crate::parser::Parser;

  #[test]
  fn test_if_else_expressions() {
    let cases = vec![
      ("if (true) { 10 }", Obj::int(10)),
      ("if (false) { 10 }", Obj::Null),
      ("if (1) { 10 }", Obj::int(10)),
      ("if (1 < 2) { 10 }", Obj::int(10)),
      ("if (1 > 2) { 10 }", Obj::Null),
      ("if (1 > 2) { 10 } else { 20 }", Obj::int(20)),
      ("if (1 < 2) { 10 } else { 20 }", Obj::int(10)),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      match (&evaluated, &expected) {
        (_, Obj::Int(int)) => assert_integer_object(evaluated, int.value),
        (Obj::Null, Obj::Null) => assert!(true),
        _ => panic!(
          "unexpected types - evaluated: {:?} expected: {:?}",
          evaluated, expected
        ),
      }
    }
  }

  #[test]
  fn test_eval_integer_expression() {
    let cases = vec![
      ("-5", -5),
      ("-10", -10),
      ("5", 5),
      ("10", 10),
      ("5 + 5 + 5 + 5 - 10", 10),
      ("2 * 2 * 2 * 2 * 2", 32),
      ("-50 + 100 + -50", 0),
      ("5 * 2 + 10", 20),
      ("5 + 2 * 10", 25),
      ("20 + 2 * -10", 0),
      ("50 / 2 * 2 + 10", 60),
      ("2 * (5 + 10)", 30),
      ("3 * 3 * 3 + 10", 37),
      ("3 * (3 * 3) + 10", 37),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      assert_integer_object(evaluated, expected);
    }
  }

  #[test]
  fn test_eval_boolean_expression() {
    let cases = vec![
      ("true", true),
      ("false", false),
      ("1 < 2", true),
      ("1 > 2", false),
      ("1 < 1", false),
      ("1 > 1", false),
      ("1 == 1", true),
      ("1 != 1", false),
      ("1 == 2", false),
      ("1 != 2", true),
      ("true == true", true),
      ("false == false", true),
      ("true == false", false),
      ("true != false", true),
      ("false != true", true),
      ("(1 < 2) == true", true),
      ("(1 < 2) == false", false),
      ("(1 > 2) == true", false),
      ("(1 > 2) == false", true),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      assert_boolean_object(evaluated, expected);
    }
  }

  #[test]
  fn test_bang_operator() {
    let cases = vec![
      ("!true", false),
      ("!false", true),
      ("!5", false),
      ("!!5", true),
      ("!!true", true),
      ("!!false", false),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      assert_boolean_object(evaluated, expected);
    }
  }

  fn assert_boolean_object(obj: Obj, expected: bool) {
    if let Obj::Bool(boolean) = obj {
      assert_eq!(boolean.value, expected);
    } else {
      panic!("object is not Boolean. got={:?}", obj);
    }
  }

  fn assert_integer_object(obj: Obj, expected: i64) {
    if let Obj::Int(int) = obj {
      assert_eq!(int.value, expected);
    } else {
      panic!("object is not Integer. got={:?}", obj);
    }
  }

  fn test_eval(input: &str) -> Obj {
    let lexer = Lexer::from(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    eval(Node::Prog(program))
  }
}
