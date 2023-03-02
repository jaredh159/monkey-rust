use std::rc::Rc;

use crate::object::{Env, Function, Integer, Obj, ReturnValue, StringObj};
use crate::parser::{BlockStatement, Either, Expr, Identifier, IfExpression, Program, Statement};

pub enum Node {
  Prog(Program),
  Stmt(Statement),
  Expr(Expr),
}

impl Node {
  fn eval_map<F>(self, env: &mut Env, f: F) -> Obj
  where
    F: FnOnce(Obj) -> Obj,
  {
    match eval(self, env) {
      Obj::Err(err) => Obj::Err(err),
      obj => f(obj),
    }
  }
}

// todo: use const members for true false
// maybe todo: refactor eval and friends to always return Result?

pub fn eval(node: Node, env: &mut Env) -> Obj {
  match node {
    Node::Prog(program) => eval_program(program, env),
    Node::Stmt(Statement::Expression(_, expr)) => eval(Node::Expr(expr), env),
    Node::Stmt(Statement::Let(_, identifier, expr)) => {
      let value = eval(Node::Expr(expr), env);
      if value.is_err() {
        return value;
      }
      env.set(identifier.value, value);
      Obj::Null
    }
    Node::Stmt(Statement::Return(_, expr)) => {
      Node::Expr(expr).eval_map(env, |value| Obj::Return(Box::new(ReturnValue { value })))
    }
    Node::Stmt(Statement::Block(block)) => eval_block_statement(block, env),
    Node::Expr(Expr::Bool(boolean)) => Obj::bool(boolean.value),
    Node::Expr(Expr::String(string)) => Obj::Str(StringObj {
      value: string.value,
    }),
    Node::Expr(Expr::Call(call)) => {
      let fn_node = match call.function {
        Either::Left(ident) => Node::Expr(Expr::Ident(ident)),
        Either::Right(fn_lit) => Node::Expr(Expr::Func(fn_lit)),
      };
      let function = eval(fn_node, env);
      if function.is_err() {
        return function;
      }
      let args = match eval_expressions(call.arguments, env) {
        Ok(args) => args,
        Err(err) => return err,
      };
      apply_function(function, args)
    }
    Node::Expr(Expr::Func(fn_lit)) => Obj::Func(Function {
      params: fn_lit.parameters,
      body: fn_lit.body,
      env: Rc::new(env.clone()), // todo
    }),
    Node::Expr(Expr::Ident(ident)) => eval_identifier(ident, env),
    Node::Expr(Expr::If(if_expr)) => eval_if_expression(if_expr, env),
    Node::Expr(Expr::Int(int)) => Obj::int(int.value),
    Node::Expr(Expr::Prefix(p)) => {
      Node::Expr(*p.rhs).eval_map(env, |rhs| eval_prefix_expr(&p.operator, rhs))
    }
    Node::Expr(Expr::Infix(infix)) => {
      let rhs = eval(Node::Expr(*infix.rhs), env);
      if rhs.is_err() {
        return rhs;
      }
      let lhs = eval(Node::Expr(*infix.lhs), env);
      if rhs.is_err() {
        return rhs;
      }
      eval_infix_expr(lhs, infix.operator, rhs)
    }
  }
}

fn apply_function(obj: Obj, args: Vec<Obj>) -> Obj {
  if let Obj::Func(function) = obj {
    let mut fn_env = extended_function_env(&function, args);
    let evaluated = eval(Node::Stmt(Statement::Block(function.body)), &mut fn_env);
    unrap_return_value(evaluated)
  } else {
    Obj::err(format!("not a function: {}", obj.type_string()))
  }
}

fn unrap_return_value(obj: Obj) -> Obj {
  if let Obj::Return(return_value) = obj {
    return_value.value
  } else {
    obj
  }
}

fn extended_function_env(function: &Function, args: Vec<Obj>) -> Env {
  let mut env = Env::new_enclosed(function.env.clone());
  for (param, arg) in function.params.iter().zip(args) {
    env.set(param.value.clone(), arg);
  }
  env
}

fn eval_expressions(exprs: Vec<Expr>, env: &mut Env) -> Result<Vec<Obj>, Obj> {
  let mut objects = Vec::new();
  for expr in exprs {
    let evaluated = eval(Node::Expr(expr), env);
    if evaluated.is_err() {
      return Err(evaluated);
    }
    objects.push(evaluated);
  }

  return Ok(objects);
}

fn eval_identifier(ident: Identifier, env: &mut Env) -> Obj {
  match env.get(&ident.value) {
    Some(val) => val.clone(),
    None => Obj::err(format!("identifier not found: {}", ident.value)),
  }
}

fn eval_infix_expr(lhs: Obj, operator: String, rhs: Obj) -> Obj {
  match (lhs, operator.as_ref(), rhs) {
    (Obj::Int(lhs), _, Obj::Int(rhs)) => eval_integer_infix_expression(lhs, operator, rhs),
    (Obj::Bool(lhs), "==", Obj::Bool(rhs)) => Obj::bool(lhs.value == rhs.value),
    (Obj::Bool(lhs), "!=", Obj::Bool(rhs)) => Obj::bool(lhs.value != rhs.value),
    (Obj::Str(lhs), "+", Obj::Str(rhs)) => Obj::Str(StringObj {
      value: format!("{}{}", lhs.value, rhs.value),
    }),
    (lhs, operator, rhs) => {
      if lhs.type_string() != rhs.type_string() {
        Obj::err(format!(
          "type mismatch: {} {} {}",
          lhs.type_string(),
          operator,
          rhs.type_string()
        ))
      } else {
        Obj::err(format!(
          "unknown operator: {} {} {}",
          lhs.type_string(),
          operator,
          rhs.type_string()
        ))
      }
    }
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
    op => Obj::err(format!("unknown operator: Obj::Int {} Obj::Int", op)),
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
    rhs => Obj::err(format!("unknown operator: -{}", rhs.type_string())),
  }
}

fn eval_if_expression(if_expr: IfExpression, env: &mut Env) -> Obj {
  let condition = eval(Node::Expr(*if_expr.condition), env);
  if condition.is_err() {
    condition
  } else if condition.is_truthy() {
    eval(Node::Stmt(Statement::Block(if_expr.consequence)), env)
  } else if if_expr.alternative.is_some() {
    eval(
      Node::Stmt(Statement::Block(if_expr.alternative.unwrap())),
      env,
    )
  } else {
    Obj::Null
  }
}

fn eval_program(program: Program, env: &mut Env) -> Obj {
  let mut result = Obj::Null;
  for stmt in program {
    result = eval(Node::Stmt(stmt), env);
    if let Obj::Return(return_value) = &result {
      return return_value.value.clone();
    } else if let Obj::Err(_) = &result {
      return result;
    }
  }
  result
}

fn eval_block_statement(block: BlockStatement, env: &mut Env) -> Obj {
  let mut result = Obj::Null;
  for stmt in block.statements {
    result = eval(Node::Stmt(stmt), env);
    if let Obj::Return(_) | Obj::Err(_) = &result {
      return result;
    }
  }
  result
}

// tests

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::Lexer;
  use crate::parser::{Node as ParserNode, Parser};

  #[test]
  fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;
    let evaluated = test_eval(input);
    if let Obj::Str(str_lit) = evaluated {
      assert_eq!(str_lit.value, "Hello World!");
    } else {
      panic!("object is not a String. got={:?}", evaluated);
    }
  }

  #[test]
  fn test_string_literal() {
    let evaluated = test_eval("\"Hello World!\"");
    if let Obj::Str(str_lit) = evaluated {
      assert_eq!(str_lit.value, "Hello World!");
    } else {
      panic!("object is not a String. got={:?}", evaluated);
    }
  }

  #[test]
  fn test_closures() {
    let input = r#"
    let newAdder = fn(x) {
      fn(y) { x + y };
    }
    let addTwo = newAdder(2);
    addTwo(2);
    "#;
    assert_integer_object(test_eval(input), 4);
  }

  #[test]
  fn test_function_application() {
    let cases = vec![
      ("let identity = fn(x) { x; }; identity(5);", 5),
      ("let identity = fn(x) { return x; }; identity(5);", 5),
      ("let double = fn(x) { x * 2; }; double(5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
      ("fn(x) { x; }(5)", 5),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      assert_integer_object(evaluated, expected);
    }
  }

  #[test]
  fn test_function_object() {
    let evaluated = test_eval("fn(x) { x + 2; };");
    if let Obj::Func(function) = evaluated {
      assert_eq!(function.params.len(), 1);
      assert_eq!(function.params[0].string(), "x");
      assert_eq!(function.body.string(), "(x + 2)");
    } else {
      panic!("object is not a Function. got={:?}", evaluated);
    }
  }

  #[test]
  fn test_let_statements() {
    let cases = vec![
      ("let a = 5; a;", 5),
      ("let a = 5 * 5; a;", 25),
      ("let a = 5; let b = a; b;", 5),
      ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      assert_integer_object(evaluated, expected);
    }
  }

  #[test]
  fn test_error_handling() {
    let cases = vec![
      (
        r#""Hello" - "World!""#,
        "unknown operator: Obj::Str - Obj::Str",
      ),
      ("foobar", "identifier not found: foobar"),
      ("5 + true;", "type mismatch: Obj::Int + Obj::Bool"),
      ("5 + true; 5;", "type mismatch: Obj::Int + Obj::Bool"),
      ("-true", "unknown operator: -Obj::Bool"),
      ("true + false;", "unknown operator: Obj::Bool + Obj::Bool"),
      (
        "5; true + false; 5",
        "unknown operator: Obj::Bool + Obj::Bool",
      ),
      (
        "if (10 > 1) { true + false; }",
        "unknown operator: Obj::Bool + Obj::Bool",
      ),
      (
        r#"
        if (10 > 1) {
          if (10 > 1) {
            return true + false;
          }

          return 1;
        }
        "#,
        "unknown operator: Obj::Bool + Obj::Bool",
      ),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      if let Obj::Err(err) = evaluated {
        assert_eq!(err.message, expected);
      } else {
        panic!("object is not Error. got={:?}", evaluated);
      }
    }
  }

  #[test]
  fn test_return_statements() {
    let cases = vec![
      ("return 10;", 10),
      ("return 10; 9;", 10),
      ("return 2 * 5; 9;", 10),
      ("9; return 2 * 5; 9;", 10),
      ("if (10 > 1) { if ( 10 > 1) { return 10; } return 1; }", 10),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      assert_integer_object(evaluated, expected);
    }
  }

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
    let mut env = Env::new();
    eval(Node::Prog(program), &mut env)
  }
}
