use crate::parser::{BlockStatement, Identifier, Node};
use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug)]
pub enum Obj {
  Int(Integer),
  Bool(Boolean),
  Return(Box<ReturnValue>),
  Func(Function),
  Err(Error),
  Str(StringObj),
  Null,
}

pub trait Object {
  fn inspect(&self) -> String;
}

#[derive(Clone, Debug)]
pub struct StringObj {
  pub value: String,
}

impl Object for StringObj {
  fn inspect(&self) -> String {
    self.value.to_string()
  }
}

#[derive(Clone, Debug)]
pub struct Function {
  pub params: Vec<Identifier>,
  pub body: BlockStatement,
  pub env: Rc<Env>,
}

impl Object for Function {
  fn inspect(&self) -> String {
    format!(
      "fn({}) {{\n{}\n}}",
      self
        .params
        .iter()
        .map(|ident| ident.string())
        .collect::<Vec<_>>()
        .join(", "),
      self.body.string()
    )
  }
}

#[derive(Clone, Debug)]
pub struct Integer {
  pub value: i64,
}

impl Object for Integer {
  fn inspect(&self) -> String {
    self.value.to_string()
  }
}

#[derive(Clone, Debug)]
pub struct Boolean {
  pub value: bool,
}

impl Object for Boolean {
  fn inspect(&self) -> String {
    self.value.to_string()
  }
}

#[derive(Clone, Debug)]
pub struct ReturnValue {
  pub value: Obj,
}

impl Object for ReturnValue {
  fn inspect(&self) -> String {
    self.value.inspect()
  }
}

#[derive(Clone, Debug)]
pub struct Error {
  pub message: String,
}

impl Object for Error {
  fn inspect(&self) -> String {
    format!("ERROR: {}", self.message)
  }
}

impl Obj {
  pub fn bool(value: bool) -> Obj {
    Obj::Bool(Boolean { value })
  }
  pub fn int(value: i64) -> Obj {
    Obj::Int(Integer { value })
  }
  pub fn err(message: String) -> Obj {
    Obj::Err(Error { message })
  }
  pub fn type_string(&self) -> &'static str {
    match self {
      Obj::Int(_) => "Obj::Int",
      Obj::Bool(_) => "Obj::Bool",
      Obj::Return(_) => "Obj::Return",
      Obj::Err(_) => "Obj::Err",
      Obj::Func(_) => "Obj::Func",
      Obj::Str(_) => "Obj::Str",
      Obj::Null => "Obj::Null",
    }
  }
  pub fn is_err(&self) -> bool {
    match self {
      Obj::Err(_) => true,
      _ => false,
    }
  }
  pub fn is_truthy(&self) -> bool {
    match self {
      Obj::Null => false,
      Obj::Bool(b) => b.value,
      _ => true,
    }
  }
}

impl Object for Obj {
  fn inspect(&self) -> String {
    match self {
      Obj::Int(int) => int.inspect(),
      Obj::Bool(boolean) => boolean.inspect(),
      Obj::Err(err) => err.inspect(),
      Obj::Return(return_value) => return_value.inspect(),
      Obj::Null => "null".to_string(),
      Obj::Func(function) => function.inspect(),
      Obj::Str(string) => string.inspect(),
    }
  }
}

#[derive(Clone, Debug)]
pub struct Env {
  pub store: HashMap<String, Obj>,
  pub outer: Option<Rc<Env>>,
}

impl Env {
  pub fn new() -> Env {
    Env {
      store: HashMap::new(),
      outer: None,
    }
  }

  pub fn new_enclosed(outer: Rc<Env>) -> Env {
    Env {
      store: HashMap::new(),
      outer: Some(outer),
    }
  }

  pub fn get(&self, name: &String) -> Option<&Obj> {
    let obj = self.store.get(name);
    if obj.is_some() {
      obj
    } else if let Some(outer) = &self.outer {
      outer.get(name)
    } else {
      None
    }
  }

  pub fn set(&mut self, name: String, value: Obj) {
    self.store.insert(name, value);
  }
}
