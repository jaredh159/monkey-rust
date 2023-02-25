#[derive(Clone, Debug)]
pub enum Obj {
  Int(Integer),
  Bool(Boolean),
  Return(Box<ReturnValue>),
  Err(Error),
  Null,
}

pub trait Object {
  fn inspect(&self) -> String;
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
    }
  }
}
