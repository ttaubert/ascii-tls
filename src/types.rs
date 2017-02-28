// TODO
pub struct Record {
  name: String,
  value: Vec<u8>
}

impl<'a> Record {
  pub fn new(name: String, value: Vec<u8>) -> Record {
    Record {
      name: name,
      value: value
    }
  }

  pub fn name(&'a self) -> &'a str {
    &self.name[..]
  }

  pub fn value(&'a self) -> &'a [u8] {
    &self.value[..]
  }
}

// TODO
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Type {
  Uint(u8),
  Opaque(u8)
}

// TODO
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Field {
  typ: Type,
  value: Vec<u8>
}

impl<'a> Field {
  pub fn new(typ: Type, value: Vec<u8>) -> Field {
    Field {
      typ: typ,
      value: value
    }
  }

  pub fn typ(&'a self) -> &'a Type {
    &self.typ
  }

  pub fn value(&'a self) -> &'a [u8] {
    &self.value[..]
  }
}
