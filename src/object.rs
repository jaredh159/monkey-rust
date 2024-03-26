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
      Self::Int(int) => int.inspect(),
      Self::Bool(boolean) => boolean.inspect(),
      Self::Str(string) => string.inspect(),
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
  Puts,
}

impl BuiltinFn {
  pub fn new_from_ident(ident: &Identifier) -> Option<Self> {
    match ident.value.as_ref() {
      "len" => Some(Self::Len),
      "first" => Some(Self::First),
      "last" => Some(Self::Last),
      "rest" => Some(Self::Rest),
      "push" => Some(Self::Push),
      "puts" => Some(Self::Puts),
      _ => None,
    }
  }
  pub fn call(&self, args: &[Obj]) -> Obj {
    match self {
      Self::Len => len(args),
      Self::First => first(args),
      Self::Last => last(args),
      Self::Rest => rest(args),
      Self::Push => push(args),
      Self::Puts => puts(args),
    }
  }
}

impl Object for BuiltinFn {
  fn inspect(&self) -> String {
    match self {
      Self::Len => "builtin function `len`".to_string(),
      Self::First => "builtin function `first`".to_string(),
      Self::Last => "builtin function `last`".to_string(),
      Self::Rest => "builtin function `rest`".to_string(),
      Self::Push => "builtin function `push`".to_string(),
      Self::Puts => "builtin function `puts`".to_string(),
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
        .map(Object::inspect)
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
        .map(Node::string)
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
  pub const fn bool(value: bool) -> Self {
    Self::Bool(Boolean { value })
  }

  pub const fn int(value: i64) -> Self {
    Self::Int(Integer { value })
  }

  pub const fn err(message: String) -> Self {
    Self::Err(Error { message })
  }

  pub const fn type_string(&self) -> &'static str {
    match self {
      Self::Array(_) => "Obj::Array",
      Self::Int(_) => "Obj::Int",
      Self::Bool(_) => "Obj::Bool",
      Self::Return(_) => "Obj::Return",
      Self::Err(_) => "Obj::Err",
      Self::Func(_) => "Obj::Func",
      Self::Str(_) => "Obj::Str",
      Self::Builtin(_) => "Obj::Builtin",
      Self::Null => "Obj::Null",
      Self::Hash(_) => "Obj::Hash",
    }
  }

  pub const fn is_err(&self) -> bool {
    matches!(self, Self::Err(_))
  }

  pub const fn is_truthy(&self) -> bool {
    match self {
      Self::Null => false,
      Self::Bool(b) => b.value,
      _ => true,
    }
  }
  pub fn hash_key(&self) -> Option<HashKey> {
    match self {
      Self::Int(int) => Some(HashKey::Int(int.clone())),
      Self::Bool(boolean) => Some(HashKey::Bool(boolean.clone())),
      Self::Str(string) => Some(HashKey::Str(string.clone())),
      _ => None,
    }
  }
}

impl Object for Obj {
  fn inspect(&self) -> String {
    match self {
      Self::Array(array) => array.inspect(),
      Self::Int(int) => int.inspect(),
      Self::Bool(boolean) => boolean.inspect(),
      Self::Err(err) => err.inspect(),
      Self::Return(return_value) => return_value.inspect(),
      Self::Null => "null".to_string(),
      Self::Func(function) => function.inspect(),
      Self::Str(string) => string.inspect(),
      Self::Builtin(builtin) => builtin.inspect(),
      Self::Hash(hash) => hash.inspect(),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Env {
  pub store: HashMap<String, Obj>,
  pub outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
  pub fn new() -> Self {
    Self { store: HashMap::new(), outer: None }
  }

  pub fn new_enclosed(outer: Rc<RefCell<Self>>) -> Self {
    Self {
      store: HashMap::new(),
      outer: Some(outer),
    }
  }

  pub fn get(&self, name: &String) -> Option<Obj> {
    self.store.get(name).map_or_else(
      || {
        self
          .outer
          .as_ref()
          .map_or_else(|| None, |outer| outer.borrow().get(name))
      },
      |obj| Some(obj.clone()),
    )
  }

  pub fn set(&mut self, name: String, value: Obj) {
    self.store.insert(name, value);
  }
}

fn first(args: &[Obj]) -> Obj {
  let arg = match single_arg(args) {
    Ok(arg) => arg,
    Err(err) => return err,
  };
  if let Obj::Array(array) = arg {
    if array.elements.is_empty() {
      Obj::Null
    } else {
      array.elements[0].clone()
    }
  } else {
    Obj::err(format!(
      "argument to `first` must be Array, got {}",
      arg.type_string()
    ))
  }
}

fn last(args: &[Obj]) -> Obj {
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

fn rest(args: &[Obj]) -> Obj {
  let arg = match single_arg(args) {
    Ok(arg) => arg,
    Err(err) => return err,
  };
  if let Obj::Array(array) = arg {
    if array.elements.is_empty() {
      Obj::Null
    } else {
      let elements = &array.elements[1..];
      Obj::Array(Array { elements: elements.to_vec() })
    }
  } else {
    Obj::err(format!(
      "argument to `rest` must be Array, got {}",
      arg.type_string()
    ))
  }
}

fn len(args: &[Obj]) -> Obj {
  let arg = match single_arg(args) {
    Ok(arg) => arg,
    Err(err) => return err,
  };
  match arg {
    Obj::Str(string) => Obj::int(i64::try_from(string.value.len()).unwrap()),
    Obj::Array(array) => Obj::int(i64::try_from(array.elements.len()).unwrap()),
    _ => Obj::err(format!(
      "argument to `len` not supported, got {}",
      arg.type_string()
    )),
  }
}

fn push(args: &[Obj]) -> Obj {
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

fn puts(args: &[Obj]) -> Obj {
  for arg in args {
    println!("{}", arg.inspect());
  }
  Obj::Null
}

fn single_arg(args: &[Obj]) -> Result<Obj, Obj> {
  if args.len() != 1 {
    return Err(Obj::err(format!(
      "wrong number of arguments. got={}, want=1",
      args.len()
    )));
  }
  Ok(args[0].clone())
}
