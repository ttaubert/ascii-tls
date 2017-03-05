use byteorder::{BigEndian, WriteBytesExt};

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
#[derive(Debug, PartialEq)]
pub enum Type {
  Uint(u8),
  Opaque(u8)
}

// A parsed value denoting a value type to allow better interpretation.
#[derive(Debug, PartialEq)]
pub enum Value {
  Block(Vec<u8>),
  Hex(Vec<u8>),
  Number(Vec<u8>),
  String(Vec<u8>)
}

// TODO
#[derive(Debug, PartialEq)]
pub struct Field {
  typ: Type,
  value: Value
}

impl<'a> Field {
  pub fn new(typ: Type, value: Value) -> Field {
    Field {
      typ: typ,
      value: value
    }
  }

  pub fn typ(&'a self) -> &'a Type {
    &self.typ
  }

  pub fn value(&'a self) -> &'a Value {
    &self.value
  }

  pub fn to_bytes(self) -> Option<Vec<u8>> {
    match self.typ {
      Type::Uint(w) => self.to_uint_bytes(w as usize),
      Type::Opaque(w) => self.to_opaque_bytes(w as usize)
    }
  }

  fn to_uint_bytes(self, width: usize) -> Option<Vec<u8>> {
    // Invariant enforced by the parser.
    assert!(width <= 8);

    let v = match self.value {
      // Block, Hex, and String values must not exceed the given type.
      Value::Block(ref v) | Value::Hex(ref v) | Value::String(ref v) => {
        if v.len() > width {
          return None;
        }

        // Prepend zeros, if necessary.
        let mut padded = vec!(0; width - v.len());
        padded.extend(v);
        padded
      }
      Value::Number(ref v) => {
        // Numbers are always parsed as u64.
        assert_eq!(v.len(), 8);

        // Check that the type is big enough to hold the given value.
        let pos = v.iter().position(|&x| x > 0);
        if pos.is_some() && v.len() - pos.unwrap() > width {
          return None;
        }

        // Truncate if necessary.
        v.iter().skip(8 - width).cloned().collect()
      }
    };

    assert_eq!(v.len(), width);
    Some(v)
  }

  fn to_opaque_bytes(self, width: usize) -> Option<Vec<u8>> {
    // Invariant enforced by the parser.
    assert!(width <= 8);

    let mut v : Vec<u8> = match self.value {
      Value::Block(ref v) | Value::Hex(ref v) | Value::String(ref v) => {
        if width > 0 {
          // Payload length must fit into the length prefix.
          if v.len() >= 2usize.pow(width as u32 * 8) {
            return None;
          }
        }

        v.iter().cloned().collect()
      },
      Value::Number(ref v) => {
        // Numbers are always parsed as u64.
        assert_eq!(v.len(), 8);

        // Remove leading zeros.
        let n = match v.iter().position(|&x| x > 0) {
          Some(pos) => pos,
          None => 7
        };

        v.iter().skip(n).cloned().collect()
      }
    };

    if width > 0 {
      // Write length prefix.
      let mut prefixed = vec!();
      if prefixed.write_uint::<BigEndian>(v.len() as u64, width).is_err() {
        return None;
      }

      prefixed.extend(v);
      v = prefixed;
    }

    Some(v)
  }
}

#[cfg(test)]
mod tests {
  use super::{Field, Type, Value};
  use std::iter;

  fn eq_value(t: Type, v: Value, res: Option<Vec<u8>>) {
    assert_eq!(Field::new(t, v).to_bytes(), res);
  }

  fn eq_uint(w: u8, v: Value, res: Option<Vec<u8>>) {
    eq_value(Type::Uint(w), v, res);
  }

  fn eq_uint_string(w: u8, inp: Vec<u8>, res: Option<Vec<u8>>) {
    eq_uint(w, Value::String(inp), res);
  }

  fn eq_uint_hex(w: u8, inp: Vec<u8>, res: Option<Vec<u8>>) {
    eq_uint(w, Value::Hex(inp), res);
  }

  fn eq_uint_block(w: u8, inp: Vec<u8>, res: Option<Vec<u8>>) {
    eq_uint(w, Value::Block(inp), res);
  }

  fn eq_uint_number(w: u8, inp: Vec<u8>, res: Option<Vec<u8>>) {
    eq_uint(w, Value::Number(inp), res);
  }

  fn eq_opaque(w: u8, v: Value, res: Option<Vec<u8>>) {
    eq_value(Type::Opaque(w), v, res);
  }

  fn eq_opaque_string(w: u8, inp: Vec<u8>, res: Option<Vec<u8>>) {
    eq_opaque(w, Value::String(inp), res);
  }

  fn eq_opaque_hex(w: u8, inp: Vec<u8>, res: Option<Vec<u8>>) {
    eq_opaque(w, Value::Hex(inp), res);
  }

  fn eq_opaque_block(w: u8, inp: Vec<u8>, res: Option<Vec<u8>>) {
    eq_opaque(w, Value::Block(inp), res);
  }

  fn eq_opaque_number(w: u8, inp: Vec<u8>, res: Option<Vec<u8>>) {
    eq_opaque(w, Value::Number(inp), res);
  }

  #[test]
  fn test_uint_strings() {
    // uint8 foo = "";
    eq_uint_string(1, vec!(), Some(vec!(0)));
    // uint16 foo = "";
    eq_uint_string(2, vec!(), Some(vec!(0, 0)));

    // uint8 foo = "a";
    eq_uint_string(1, vec!(97), Some(vec!(97)));
    // uint16 foo = "a";
    eq_uint_string(2, vec!(97), Some(vec!(0, 97)));

    // uint8 foo = "aa";
    eq_uint_string(1, vec!(97, 97), None);
    // uint16 foo = "aa";
    eq_uint_string(2, vec!(97, 97), Some(vec!(97, 97)));

    // uint16 foo = "aaa";
    eq_uint_string(2, vec!(97, 97, 97), None);
  }

  #[test]
  fn test_uint_hex() {
    // uint8 foo = 0x61;
    eq_uint_hex(1, vec!(97), Some(vec!(97)));
    // uint16 foo = 0x61;
    eq_uint_hex(2, vec!(97), Some(vec!(0, 97)));

    // uint8 foo = 0x61, 0x61;
    eq_uint_hex(1, vec!(97, 97), None);
    // uint16 foo = 0x61, 0x61;
    eq_uint_hex(2, vec!(97, 97), Some(vec!(97, 97)));

    // uint16 foo = 0x61, 0x61, 0x61;
    eq_uint_hex(2, vec!(97, 97, 97), None);
  }

  #[test]
  fn test_uint_blocks() {
    // uint8 foo = {}
    eq_uint_block(1, vec!(), Some(vec!(0)));

    // uint8 foo = { uint8 bar = "a"; }
    eq_uint_block(1, vec!(97), Some(vec!(97)));

    // uint8 foo = { uint16 bar = 97; }
    eq_uint_block(1, vec!(0, 97), None);

    // uint16 foo = { uint16 bar = 97; }
    eq_uint_block(2, vec!(0, 97), Some(vec!(0, 97)));

    // uint16 foo = { uint8 bar = 97; uint16 baz = 97; }
    eq_uint_block(2, vec!(97, 0, 97), None);
  }

  #[test]
  fn test_uint_numbers() {
    // uint8 foo = 97;
    eq_uint_number(1, vec!(0, 0, 0, 0, 0, 0, 0, 97), Some(vec!(97)));
    // uint16 foo = 97;
    eq_uint_number(2, vec!(0, 0, 0, 0, 0, 0, 0, 97), Some(vec!(0, 97)));

    // uint8 foo = 24929;
    eq_uint_number(1, vec!(0, 0, 0, 0, 0, 0, 97, 97), None);
    // uint16 foo = 24929;
    eq_uint_number(2, vec!(0, 0, 0, 0, 0, 0, 97, 97), Some(vec!(97, 97)));

    // uint16 foo = 6381921;
    eq_uint_number(2, vec!(0, 0, 0, 0, 0, 97, 97, 97), None);

    // uint64 foo = 9223372036854775808;
    eq_uint_number(8, vec!(128, 0, 0, 0, 0, 0, 0, 0),
                      Some(vec!(128, 0, 0, 0, 0, 0, 0, 0)));
    // uint64 foo = 18446744073709551615;
    eq_uint_number(8, vec!(255, 255, 255, 255, 255, 255, 255, 255),
                      Some(vec!(255, 255, 255, 255, 255, 255, 255, 255)));
  }

  #[test]
  fn test_opaque_strings() {
    // opaque foo = "";
    eq_opaque_string(0, vec!(), Some(vec!()));

    // opaque foo = "a";
    eq_opaque_string(0, vec!(97), Some(vec!(97)));

    // opaque foo = "aa";
    eq_opaque_string(0, vec!(97, 97), Some(vec!(97, 97)));

    // opaque foo = "aaa";
    eq_opaque_string(0, vec!(97, 97, 97), Some(vec!(97, 97, 97)));

    // opaque(uint8) foo = "";
    eq_opaque_string(1, vec!(), Some(vec!(0)));

    // opaque(uint8) foo = "a";
    eq_opaque_string(1, vec!(97), Some(vec!(1, 97)));

    // opaque(uint8) foo = "a" * 255;
    let inp : Vec<u8> = iter::repeat(97).take(255).collect();
    let res = vec!(255).into_iter().chain(inp.iter().cloned()).collect();
    eq_opaque_string(1, inp, Some(res));

    // opaque(uint8) foo = "a" * 256;
    eq_opaque_string(1, iter::repeat(97).take(256).collect(), None);
  }

  #[test]
  fn test_opaque_hex() {
    // opaque foo = 0x61;
    eq_opaque_hex(0, vec!(97), Some(vec!(97)));

    // opaque foo = 0x61, 0x61;
    eq_opaque_hex(0, vec!(97, 97), Some(vec!(97, 97)));

    // opaque foo = 0x61, 0x61, 0x61;
    eq_opaque_hex(0, vec!(97, 97, 97), Some(vec!(97, 97, 97)));

    // opaque(uint8) foo = 0x61;
    eq_opaque_hex(1, vec!(97), Some(vec!(1, 97)));

    // opaque(uint8) foo = 0x61 * 255;
    let inp : Vec<u8> = iter::repeat(97).take(255).collect();
    let res = vec!(255).into_iter().chain(inp.iter().cloned()).collect();
    eq_opaque_hex(1, inp, Some(res));

    // opaque(uint8) foo = 0x61 * 256;
    eq_opaque_hex(1, iter::repeat(97).take(256).collect(), None);
  }

  #[test]
  fn test_opaque_blocks() {
    // opaque foo = {};
    eq_opaque_block(0, vec!(), Some(vec!()));

    // opaque foo = { uint8 bar = 97 };
    eq_opaque_block(0, vec!(97), Some(vec!(97)));

    // opaque foo = { uint8 bar = 97; uint8 baz = 97 };
    eq_opaque_block(0, vec!(97, 97), Some(vec!(97, 97)));

    // opaque foo = { uint8 bar = 97; uint8 baz = 97; uint8 bax = 97 };
    eq_opaque_block(0, vec!(97, 97, 97), Some(vec!(97, 97, 97)));

    // opaque(uint8) foo = { uint8 bar = 97 };
    eq_opaque_block(1, vec!(97), Some(vec!(1, 97)));

    // opaque(uint8) foo = { uint8 bar = 97 /* x 255 */ };
    let inp : Vec<u8> = iter::repeat(97).take(255).collect();
    let res = vec!(255).into_iter().chain(inp.iter().cloned()).collect();
    eq_opaque_block(1, inp, Some(res));

    // opaque(uint8) foo = { uint8 bar = 97 /* x 256 */ };
    eq_opaque_block(1, iter::repeat(97).take(256).collect(), None);
  }

  #[test]
  fn test_opaque_numbers() {
    // opaque foo = 97;
    eq_opaque_number(0, vec!(0, 0, 0, 0, 0, 0, 0, 97), Some(vec!(97)));

    // opaque foo = 24929;
    eq_opaque_number(0, vec!(0, 0, 0, 0, 0, 0, 97, 97), Some(vec!(97, 97)));

    // opaque foo = 9223372036854775808;
    eq_opaque_number(0, vec!(128, 0, 0, 0, 0, 0, 0, 0),
                        Some(vec!(128, 0, 0, 0, 0, 0, 0, 0)));
    // opaque foo = 18446744073709551615;
    eq_opaque_number(0, vec!(255, 255, 255, 255, 255, 255, 255, 255),
                        Some(vec!(255, 255, 255, 255, 255, 255, 255, 255)));

    // opaque(uint8) foo = 97;
    eq_opaque_number(1, vec!(0, 0, 0, 0, 0, 0, 0, 97), Some(vec!(1, 97)));

    // opaque(uint8) foo = 24929;
    eq_opaque_number(1, vec!(0, 0, 0, 0, 0, 0, 97, 97), Some(vec!(2, 97, 97)));

    // opaque(uint16) = 18446744073709551615;
    eq_opaque_number(2, vec!(255, 255, 255, 255, 255, 255, 255, 255),
                        Some(vec!(0, 8, 255, 255, 255, 255, 255, 255, 255, 255)));
  }
}
