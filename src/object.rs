use crate::parser::{BlockStatement, Identifier, Node};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Obj {
  Array(Array),
  Bool(Boolean),
  Builtin(BuiltinFn),
  Err(Error),
  Func(Function),
  Int(Integer),
  Null,
  Return(Box<ReturnValue>),
  Str(StringObj),
  Hash(Hash),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HashKey {
  Int(Integer),
  Bool(Boolean),
  Str(StringObj),
}

impl HashKey {
  pub fn inspect(&self) -> String {
    match self {
      HashKey::Int(int) => int.inspect(),
      HashKey::Bool(boolean) => boolean.inspect(),
      HashKey::Str(string) => string.inspect(),
    }
  }
}

pub trait Object {
  fn inspect(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltinFn {
  Len,
  First,
  Last,
  Rest,
  Push,
}

impl BuiltinFn {
  pub fn new_from_ident(ident: &Identifier) -> Option<Self> {
    match ident.value.as_ref() {
      "len" => Some(BuiltinFn::Len),
      "first" => Some(BuiltinFn::First),
      "last" => Some(BuiltinFn::Last),
      "rest" => Some(BuiltinFn::Rest),
      "push" => Some(BuiltinFn::Push),
      _ => None,
    }
  }
  pub fn call(&self, args: Vec<Obj>) -> Obj {
    match self {
      BuiltinFn::Len => len(args),
      BuiltinFn::First => first(args),
      BuiltinFn::Last => last(args),
      BuiltinFn::Rest => rest(args),
      BuiltinFn::Push => push(args),
    }
  }
}

impl Object for BuiltinFn {
  fn inspect(&self) -> String {
    match self {
      BuiltinFn::Len => "builtin function `len`".to_string(),
      BuiltinFn::First => "builtin function `first`".to_string(),
      BuiltinFn::Last => "builtin function `last`".to_string(),
      BuiltinFn::Rest => "builtin function `rest`".to_string(),
      BuiltinFn::Push => "builtin function `push`".to_string(),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Hash {
  pub pairs: HashMap<HashKey, Obj>,
}

impl Object for Hash {
  fn inspect(&self) -> String {
    format!(
      "{{{}}}",
      self
        .pairs
        .iter()
        .map(|(hash_key, value)| format!("{}: {}", hash_key.inspect(), value.inspect()))
        .collect::<Vec<_>>()
        .join(", "),
    )
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StringObj {
  pub value: String,
}

impl Object for StringObj {
  fn inspect(&self) -> String {
    self.value.to_string()
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Array {
  pub elements: Vec<Obj>,
}

impl Object for Array {
  fn inspect(&self) -> String {
    format!(
      "[{}]",
      self
        .elements
        .iter()
        .map(|e| e.inspect())
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
  pub params: Vec<Identifier>,
  pub body: BlockStatement,
  pub env: Rc<RefCell<Env>>,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Integer {
  pub value: i64,
}

impl Object for Integer {
  fn inspect(&self) -> String {
    self.value.to_string()
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Boolean {
  pub value: bool,
}

impl Object for Boolean {
  fn inspect(&self) -> String {
    self.value.to_string()
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturnValue {
  pub value: Obj,
}

impl Object for ReturnValue {
  fn inspect(&self) -> String {
    self.value.inspect()
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
      Obj::Array(_) => "Obj::Array",
      Obj::Int(_) => "Obj::Int",
      Obj::Bool(_) => "Obj::Bool",
      Obj::Return(_) => "Obj::Return",
      Obj::Err(_) => "Obj::Err",
      Obj::Func(_) => "Obj::Func",
      Obj::Str(_) => "Obj::Str",
      Obj::Builtin(_) => "Obj::Builtin",
      Obj::Null => "Obj::Null",
      Obj::Hash(_) => "Obj::Hash",
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
  pub fn hash_key(&self) -> Option<HashKey> {
    match self {
      Obj::Int(int) => Some(HashKey::Int(int.clone())),
      Obj::Bool(boolean) => Some(HashKey::Bool(boolean.clone())),
      Obj::Str(string) => Some(HashKey::Str(string.clone())),
      _ => None,
    }
  }
}

impl Object for Obj {
  fn inspect(&self) -> String {
    match self {
      Obj::Array(array) => array.inspect(),
      Obj::Int(int) => int.inspect(),
      Obj::Bool(boolean) => boolean.inspect(),
      Obj::Err(err) => err.inspect(),
      Obj::Return(return_value) => return_value.inspect(),
      Obj::Null => "null".to_string(),
      Obj::Func(function) => function.inspect(),
      Obj::Str(string) => string.inspect(),
      Obj::Builtin(builtin) => builtin.inspect(),
      Obj::Hash(hash) => hash.inspect(),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Env {
  pub store: HashMap<String, Obj>,
  pub outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
  pub fn new() -> Env {
    Env {
      store: HashMap::new(),
      outer: None,
    }
  }

  pub fn new_enclosed(outer: Rc<RefCell<Env>>) -> Env {
    Env {
      store: HashMap::new(),
      outer: Some(outer),
    }
  }

  pub fn get(&self, name: &String) -> Option<Obj> {
    if let Some(obj) = self.store.get(name) {
      Some(obj.clone())
    } else if let Some(outer) = &self.outer {
      outer.borrow().get(name)
    } else {
      None
    }
  }

  pub fn set(&mut self, name: String, value: Obj) {
    self.store.insert(name, value);
  }
}

fn first(args: Vec<Obj>) -> Obj {
  let arg = match single_arg(args) {
    Ok(arg) => arg,
    Err(err) => return err,
  };
  if let Obj::Array(array) = arg {
    if array.elements.len() > 0 {
      array.elements[0].clone()
    } else {
      Obj::Null
    }
  } else {
    Obj::err(format!(
      "argument to `first` must be Array, got {}",
      arg.type_string()
    ))
  }
}

fn last(args: Vec<Obj>) -> Obj {
  let arg = match single_arg(args) {
    Ok(arg) => arg,
    Err(err) => return err,
  };
  if let Obj::Array(array) = arg {
    let len = array.elements.len();
    if len > 0 {
      array.elements[len - 1].clone()
    } else {
      Obj::Null
    }
  } else {
    Obj::err(format!(
      "argument to `last` must be Array, got {}",
      arg.type_string()
    ))
  }
}

fn rest(args: Vec<Obj>) -> Obj {
  let arg = match single_arg(args) {
    Ok(arg) => arg,
    Err(err) => return err,
  };
  if let Obj::Array(array) = arg {
    if array.elements.len() > 0 {
      let foo = &array.elements[1..];
      let bar = foo.to_vec();
      Obj::Array(Array { elements: bar })
    } else {
      Obj::Null
    }
  } else {
    Obj::err(format!(
      "argument to `rest` must be Array, got {}",
      arg.type_string()
    ))
  }
}

fn len(args: Vec<Obj>) -> Obj {
  let arg = match single_arg(args) {
    Ok(arg) => arg,
    Err(err) => return err,
  };
  match arg {
    Obj::Str(string) => Obj::int(string.value.len() as i64),
    Obj::Array(array) => Obj::int(array.elements.len() as i64),
    _ => Obj::err(format!(
      "argument to `len` not supported, got {}",
      arg.type_string()
    )),
  }
}

fn push(args: Vec<Obj>) -> Obj {
  if args.len() != 2 {
    return Obj::err(format!(
      "wrong number of arguments. got={}, want=2",
      args.len()
    ));
  }
  let first_arg = &args[0];
  if let Obj::Array(array) = first_arg {
    let mut elements = array.elements.clone();
    elements.push(args[1].clone());
    Obj::Array(Array { elements })
  } else {
    Obj::err(format!(
      "first argument to `push` must be Array, got {}",
      first_arg.type_string()
    ))
  }
}

fn single_arg(args: Vec<Obj>) -> Result<Obj, Obj> {
  if args.len() != 1 {
    return Err(Obj::err(format!(
      "wrong number of arguments. got={}, want=1",
      args.len()
    )));
  }
  Ok(args[0].clone())
}
