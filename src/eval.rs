use crate::object::{Boolean, Integer, Obj};
use crate::parser::PrefixExpression;
use crate::parser::{Expr, Program, Statement};

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
    Node::Stmt(Statement::Block(_)) => todo!(),
    Node::Expr(Expr::Bool(b)) => Obj::Bool(Boolean { value: b.value }),
    Node::Expr(Expr::Call(_)) => todo!(),
    Node::Expr(Expr::Func(_)) => todo!(),
    Node::Expr(Expr::Ident(_)) => todo!(),
    Node::Expr(Expr::If(_)) => todo!(),
    Node::Expr(Expr::Infix(_)) => todo!(),
    Node::Expr(Expr::Int(i)) => Obj::Int(Integer { value: i.value }),
    Node::Expr(Expr::Prefix(p)) => eval_prefix_expr(&p.operator, eval(Node::Expr(*p.rhs))),
  }
}

fn eval_prefix_expr(operator: &str, rhs: Obj) -> Obj {
  match operator {
    "!" => eval_bang_operator_expression(rhs),
    "-" => eval_minus_prefix_operator_expression(rhs),
    _ => todo!(),
  }
}

fn eval_bang_operator_expression(rhs: Obj) -> Obj {
  match rhs {
    Obj::Bool(b) => Obj::Bool(Boolean { value: !b.value }),
    Obj::Null => Obj::Bool(Boolean { value: true }),
    _ => Obj::Bool(Boolean { value: false }),
  }
}

fn eval_minus_prefix_operator_expression(rhs: Obj) -> Obj {
  match rhs {
    Obj::Int(int) => Obj::Int(Integer { value: -int.value }),
    _ => Obj::Null,
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
  fn test_eval_integer_expression() {
    let cases = vec![("-5", -5), ("-10", -10), ("5", 5), ("10", 10)];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      assert_integer_object(evaluated, expected);
    }
  }

  #[test]
  fn test_eval_boolean_expression() {
    let cases = vec![("true", true), ("false", false)];
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
