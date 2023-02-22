#[derive(Clone, Debug)]
pub enum Obj {
  Int(Integer),
  Bool(Boolean),
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

impl Object for Obj {
  fn inspect(&self) -> String {
    match self {
      Obj::Int(int) => int.inspect(),
      Obj::Bool(boolean) => boolean.inspect(),
      Obj::Null => "null".to_string(),
    }
  }
}
