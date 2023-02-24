#[derive(Clone, Debug)]
pub enum Obj {
  Int(Integer),
  Bool(Boolean),
  Return(Box<ReturnValue>),
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

impl Obj {
  pub fn bool(value: bool) -> Obj {
    Obj::Bool(Boolean { value })
  }
  pub fn int(value: i64) -> Obj {
    Obj::Int(Integer { value })
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
      Obj::Return(return_value) => return_value.inspect(),
      Obj::Null => "null".to_string(),
    }
  }
}
