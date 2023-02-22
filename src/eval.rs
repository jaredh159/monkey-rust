use crate::object::{Integer, Obj};
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
    Node::Expr(Expr::Bool(_)) => todo!(),
    Node::Expr(Expr::Call(_)) => todo!(),
    Node::Expr(Expr::Func(_)) => todo!(),
    Node::Expr(Expr::Ident(_)) => todo!(),
    Node::Expr(Expr::If(_)) => todo!(),
    Node::Expr(Expr::Infix(_)) => todo!(),
    Node::Expr(Expr::Int(int)) => Obj::Int(Integer { value: int.value }),
    Node::Expr(Expr::Prefix(_)) => todo!(),
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
    let cases = vec![("5", 5), ("10", 10)];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      assert_integer_object(evaluated, expected);
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
