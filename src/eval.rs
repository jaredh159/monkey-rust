use crate::object::{Array, BuiltinFn, Env, Function};
use crate::object::{Hash, Integer, Obj, ReturnValue, StringObj};
use crate::parser::{BlockStatement, Either, Expr, HashLiteral};
use crate::parser::{Identifier, IfExpression, Program, Statement};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub enum Node {
  Prog(Program),
  Stmt(Statement),
  Expr(Expr),
}

impl Node {
  fn eval_map<F>(self, env: Rc<RefCell<Env>>, f: F) -> Obj
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

pub fn eval(node: Node, env: Rc<RefCell<Env>>) -> Obj {
  match node {
    Node::Prog(program) => eval_program(program, env),
    Node::Stmt(Statement::Expression(_, expr)) => eval(Node::Expr(expr), env),
    Node::Stmt(Statement::Let(_, identifier, expr)) => {
      let value = eval(Node::Expr(expr), Rc::clone(&env));
      if value.is_err() {
        return value;
      }
      {
        let mut env = env.borrow_mut();
        env.set(identifier.value, value);
      }
      Obj::Null
    }
    Node::Stmt(Statement::Return(_, expr)) => {
      Node::Expr(expr).eval_map(env, |value| Obj::Return(Box::new(ReturnValue { value })))
    }
    Node::Stmt(Statement::Block(block)) => eval_block_statement(block, env),
    Node::Expr(Expr::Bool(boolean)) => Obj::bool(boolean.value),
    Node::Expr(Expr::Hash(hash_lit)) => eval_hash_literal(hash_lit, env),
    Node::Expr(Expr::String(string)) => Obj::Str(StringObj {
      value: string.value,
    }),
    Node::Expr(Expr::Array(array)) => {
      let elements = match eval_expressions(array.elements, env) {
        Ok(elements) => elements,
        Err(err) => return err,
      };
      Obj::Array(Array { elements })
    }
    Node::Expr(Expr::Index(index)) => {
      let left = eval(Node::Expr(*index.left), Rc::clone(&env));
      if left.is_err() {
        return left;
      }
      let index = eval(Node::Expr(*index.index), env);
      if index.is_err() {
        return index;
      }
      eval_index_expr(left, index)
    }
    Node::Expr(Expr::Call(call)) => {
      let fn_node = match call.function {
        Either::Left(ident) => Node::Expr(Expr::Ident(ident)),
        Either::Right(fn_lit) => Node::Expr(Expr::Func(fn_lit)),
      };
      let function = eval(fn_node, Rc::clone(&env));
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
      env: Rc::clone(&env),
    }),
    Node::Expr(Expr::Ident(ident)) => eval_identifier(ident, env),
    Node::Expr(Expr::If(if_expr)) => eval_if_expression(if_expr, env),
    Node::Expr(Expr::Int(int)) => Obj::int(int.value),
    Node::Expr(Expr::Prefix(p)) => {
      Node::Expr(*p.rhs).eval_map(env, |rhs| eval_prefix_expr(&p.operator, rhs))
    }
    Node::Expr(Expr::Infix(infix)) => {
      let rhs = eval(Node::Expr(*infix.rhs), Rc::clone(&env));
      if rhs.is_err() {
        return rhs;
      }
      let lhs = eval(Node::Expr(*infix.lhs), env);
      if lhs.is_err() {
        return rhs;
      }
      eval_infix_expr(lhs, infix.operator, rhs)
    }
  }
}

fn eval_hash_literal(hash_lit: HashLiteral, env: Rc<RefCell<Env>>) -> Obj {
  let mut pairs = HashMap::new();
  for (key_node, value_node) in hash_lit.pairs {
    let key = eval(Node::Expr(key_node), Rc::clone(&env));
    if key.is_err() {
      return key;
    }
    let hash_key = match key.hash_key() {
      Some(hash_key) => hash_key,
      None => return Obj::err(format!("unusable as hash key: {}", key.type_string())),
    };
    let value = eval(Node::Expr(value_node), Rc::clone(&env));
    if value.is_err() {
      return value;
    }
    pairs.insert(hash_key, value);
  }
  Obj::Hash(Hash { pairs })
}

fn eval_index_expr(left: Obj, index: Obj) -> Obj {
  match (left, index) {
    (Obj::Array(array), Obj::Int(int)) => eval_array_index_expression(array, int),
    (Obj::Hash(hash), index) => eval_hash_index_expression(hash, index),
    (left, _) => Obj::err(format!(
      "index operator not supported: {}",
      left.type_string()
    )),
  }
}

fn eval_hash_index_expression(hash: Hash, index: Obj) -> Obj {
  let hash_key = match index.hash_key() {
    Some(hash_key) => hash_key,
    None => return Obj::err(format!("unusable as hash key: {}", index.type_string())),
  };
  hash
    .pairs
    .get(&hash_key)
    .map_or_else(|| Obj::Null, |val| val.clone())
}

fn eval_array_index_expression(array: Array, int: Integer) -> Obj {
  let index = int.value as usize;
  let max = array.elements.len() - 1;
  if index > max || int.value < 0 {
    return Obj::Null;
  }
  array.elements[index].clone()
}

fn apply_function(obj: Obj, args: Vec<Obj>) -> Obj {
  if let Obj::Func(function) = obj {
    let fn_env = extended_function_env(&function, args);
    let evaluated = eval(
      Node::Stmt(Statement::Block(function.body)),
      Rc::new(RefCell::new(fn_env)), // check this..
    );
    unrap_return_value(evaluated)
  } else if let Obj::Builtin(builtin) = obj {
    builtin.call(args)
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
  let mut env = Env::new_enclosed(Rc::clone(&function.env));
  for (param, arg) in function.params.iter().zip(args) {
    env.set(param.value.clone(), arg);
  }
  env
}

fn eval_expressions(exprs: Vec<Expr>, env: Rc<RefCell<Env>>) -> Result<Vec<Obj>, Obj> {
  let mut objects = Vec::new();
  for expr in exprs {
    let evaluated = eval(Node::Expr(expr), Rc::clone(&env));
    if evaluated.is_err() {
      return Err(evaluated);
    }
    objects.push(evaluated);
  }

  return Ok(objects);
}

fn eval_identifier(ident: Identifier, env: Rc<RefCell<Env>>) -> Obj {
  match env.borrow().get(&ident.value) {
    Some(val) => val.clone(),
    None => BuiltinFn::new_from_ident(&ident).map_or_else(
      || Obj::err(format!("identifier not found: {}", ident.value)),
      |b| Obj::Builtin(b),
    ),
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

fn eval_if_expression(if_expr: IfExpression, env: Rc<RefCell<Env>>) -> Obj {
  let condition = eval(Node::Expr(*if_expr.condition), Rc::clone(&env));
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

fn eval_program(program: Program, env: Rc<RefCell<Env>>) -> Obj {
  let mut result = Obj::Null;
  for stmt in program {
    result = eval(Node::Stmt(stmt), Rc::clone(&env));
    if let Obj::Return(return_value) = &result {
      return return_value.value.clone();
    } else if let Obj::Err(_) = &result {
      return result;
    }
  }
  result
}

fn eval_block_statement(block: BlockStatement, env: Rc<RefCell<Env>>) -> Obj {
  let mut result = Obj::Null;
  for stmt in block.statements {
    result = eval(Node::Stmt(stmt), Rc::clone(&env));
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
  use crate::object::{Boolean, HashKey};
  use crate::parser::{Node as ParserNode, Parser};

  #[test]
  fn test_hash_index_expressions() {
    let tests = vec![
      (r#"{"foo": 5}["foo"]"#, Obj::int(5)),
      (r#"{"foo": 5}["bar"]"#, Obj::Null),
      (r#"let key = "foo"; {"foo": 5}[key]"#, Obj::int(5)),
      (r#"{}["foo"]"#, Obj::Null),
      (r#"{5: 5}[5]"#, Obj::int(5)),
      (r#"{true: 5}[true]"#, Obj::int(5)),
      (r#"{false: 5}[false]"#, Obj::int(5)),
    ];
    for (input, expected) in tests {
      let evaluated = test_eval(input);
      if let Obj::Int(int) = expected {
        assert_integer_object(evaluated, int.value);
      } else {
        assert_eq!(evaluated, expected);
      }
    }
  }

  #[test]
  fn test_hash_literals() {
    let input = r#"
    let two = "two";
    {
        "one": 10 - 9,
        two: 1 + 1,
        "thr" + "ee": 6 / 2,
        4: 4,
        true: 5,
        false: 6
    }
    "#;
    let expected = vec![
      (
        HashKey::Str(StringObj {
          value: "one".to_string(),
        }),
        1,
      ),
      (
        HashKey::Str(StringObj {
          value: "two".to_string(),
        }),
        2,
      ),
      (
        HashKey::Str(StringObj {
          value: "three".to_string(),
        }),
        3,
      ),
      (HashKey::Int(Integer { value: 4 }), 4),
      (HashKey::Bool(Boolean { value: true }), 5),
      (HashKey::Bool(Boolean { value: false }), 6),
    ];
    let evaluated = test_eval(input);
    if let Obj::Hash(hash_lit) = evaluated {
      assert_eq!(hash_lit.pairs.len(), expected.len());
      for (key, int) in expected {
        assert_integer_object(hash_lit.pairs[&key].clone(), int);
      }
    } else {
      panic!("object is not a Hash. got={:?}", evaluated);
    }
  }

  #[test]
  fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input);
    if let Obj::Array(array) = evaluated {
      assert_eq!(array.elements.len(), 3);
      assert_integer_object(array.elements[0].clone(), 1);
      assert_integer_object(array.elements[1].clone(), 4);
      assert_integer_object(array.elements[2].clone(), 6);
    } else {
      panic!("object is not a Array. got={:?}", evaluated);
    }
  }

  #[test]
  fn test_index_expressions() {
    let cases = vec![
      ("[1, 2, 3][0]", Obj::int(1)),
      ("[1, 2, 3][1]", Obj::int(2)),
      ("[1, 2, 3][2]", Obj::int(3)),
      ("let i = 0; [1][i];", Obj::int(1)),
      ("[1, 2, 3][1 + 1];", Obj::int(3)),
      ("let myArray = [1, 2, 3]; myArray[2];", Obj::int(3)),
      (
        "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
        Obj::int(6),
      ),
      (
        "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
        Obj::int(2),
      ),
      ("[1, 2, 3][3]", Obj::Null),
      ("[1, 2, 3][-1]", Obj::Null),
    ];
    for (input, expected) in cases {
      match expected {
        Obj::Int(int) => assert_integer_object(test_eval(input), int.value),
        _ => assert!(test_eval(input).type_string() == "Obj::Null"),
      }
    }
  }

  #[test]
  fn test_builtin_functions() {
    let cases = vec![
      (r#"len("")"#, Obj::int(0)),
      (r#"len("four")"#, Obj::int(4)),
      (r#"len("hello world")"#, Obj::int(11)),
      (r#"len([1, 2, 3])"#, Obj::int(3)),
      (r#"len([1, [3, 4]])"#, Obj::int(2)),
      (r#"len([[]])"#, Obj::int(1)),
      (r#"len([])"#, Obj::int(0)),
      (r#"first([1])"#, Obj::int(1)),
      (r#"first([])"#, Obj::Null),
      (r#"first([3, 1])"#, Obj::int(3)),
      (r#"last([1])"#, Obj::int(1)),
      (r#"last([])"#, Obj::Null),
      (r#"last([3, 1])"#, Obj::int(1)),
      (r#"rest([1])"#, Obj::Array(Array { elements: vec![] })),
      (r#"rest([])"#, Obj::Null),
      (
        r#"rest([1, 2, 3])"#,
        Obj::Array(Array {
          elements: vec![Obj::int(2), Obj::int(3)],
        }),
      ),
      (
        r#"push([1], 2)"#,
        Obj::Array(Array {
          elements: vec![Obj::int(1), Obj::int(2)],
        }),
      ),
      (
        r#"push([], 1)"#,
        Obj::Array(Array {
          elements: vec![Obj::int(1)],
        }),
      ),
      (
        r#"let a = [1]; push(a, 2); a;"#,
        Obj::Array(Array {
          elements: vec![Obj::int(1)],
        }),
      ),
      (
        r#"len(1)"#,
        Obj::err("argument to `len` not supported, got Obj::Int".to_string()),
      ),
      (
        r#"len("one", "two")"#,
        Obj::err("wrong number of arguments. got=2, want=1".to_string()),
      ),
    ];
    for (input, expected) in cases {
      let evaluated = test_eval(input);
      match (evaluated, expected) {
        (evaluated, Obj::Int(int)) => assert_integer_object(evaluated, int.value),
        (evaluated, Obj::Array(array)) => assert!(evaluated == Obj::Array(array)),
        (Obj::Err(eval_err), Obj::Err(err)) => assert_eq!(eval_err.message, err.message),
        (Obj::Null, Obj::Null) => assert!(true),
        (_, expected) => panic!("unexpected eval result: {}", expected.type_string()),
      }
    }
  }

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
  fn test_recursion() {
    let input = r#"
    let foo = fn(x) {
      if (x > 0) {
        return foo(x - 1);
      }
      return x;
    }
    foo(1);
    "#;
    assert_integer_object(test_eval(input), 0);
  }

  #[test]
  fn test_map() {
    let input = r#"
    let map = fn(arr, f) {
      let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
          accumulated
        } else {
          iter(rest(arr), push(accumulated, f(first(arr))));
        }
      };
      iter(arr, []);
    };

    let a = [1, 2, 3, 4];
    let double = fn(x) { x * 2 };
    map(a, double);
    "#;

    assert_eq!(
      test_eval(input),
      Obj::Array(Array {
        elements: vec![Obj::int(2), Obj::int(4), Obj::int(6), Obj::int(8)]
      })
    );
  }

  #[test]
  fn test_reduce() {
    let input = r#"
    let reduce = fn(arr, initial, f) {
      let iter = fn(arr, result) {
        if (len(arr) == 0) {
          result
        } else {
          iter(rest(arr), f(result, first(arr)));
        }
      };

      iter(arr, initial);
    };

    let sum = fn(arr) {
      reduce(arr, 0, fn(initial, el) { initial + el });
    };

    sum([1, 2, 3, 4, 5]);
    "#;

    assert_integer_object(test_eval(input), 15);
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
      (
        r#"{"name": "Monkey"}[fn(x) { x }];"#,
        "unusable as hash key: Obj::Func",
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
    let env = Env::new();
    eval(Node::Prog(program), Rc::new(RefCell::new(env)))
  }
}
