use std::iter::FromIterator;
use std::str::from_utf8;

use byteorder::{BigEndian, WriteBytesExt};
use nom::{alphanumeric, digit, eol, multispace, not_line_ending, space};

use types::{Field, Record, Type, Value};

// A "//" to comment out the rest of the line.
named!(comment_one_line,
  do_parse!(
    tag!("//") >> not_line_ending >> alt!(eof!() | eol) >> (b"")
  )
);

// Block comments wrapped in /* ... */.
named!(comment_block,
  do_parse!(
    tag!("/*") >> take_until_and_consume!("*/") >> (b"")
  )
);

// Spaces, newlines, and comments.
named!(blanks,
  do_parse!(
    many0!(alt!(multispace | comment_one_line | comment_block)) >> (b"")
  )
);

// TODO replace \" \t \r \n \0 \\
/* Parses a string delimited by quotation marks. */
named!(string<&[u8], Value>,
  map!(
    delimited!(char!('"'), is_not!("\""), char!('"')),
    |s: &[u8]| { Value::String(s.to_vec()) }
  )
);

/* Parses a single hex value "0x.." of length in bytes >= 1. */
named!(hex_value<&[u8], Vec<u8>>,
  map!(do_parse!(
    opt!(blanks) >>
    tag!("0x") >> h:
    many1!(one_of!("0123456789abcdef")) >>
    opt!(blanks) >>
    (h)
  ), |h: Vec<char>| {
    fn convert(byte: u8) -> u8 {
      match byte {
        b'a'...b'f' => 10 + byte - b'a',
        b'A'...b'F' => 10 + byte - b'A',
        b'0'...b'9' => byte - b'0',
        _ => unreachable!()
      }
    }

    let offset = h.len() & 1;
    let chars = &h[offset..];

    let mut bytes : Vec<u8> = chars.chunks(2).map(|chunk| {
      (convert(chunk[0] as u8) << 4) + convert(chunk[1] as u8)
    }).collect();

    if offset == 1 {
      bytes.insert(0, convert(h[0] as u8));
    }

    bytes
  })
);

/* Parses a list of hex values "0x..", separated by commas. */
named!(hex<&[u8], Value>,
  map!(separated_nonempty_list!(char!(','), hex_value), |hs: Vec<Vec<u8>>| {
    Value::Hex(hs.iter().fold(vec!(), |mut acc, h| {
      acc.extend(h);
      acc
    }))
  })
);

/* A good old, plain number. 64 bits max. */
named!(number<&[u8], Value>,
  map_res!(map_res!(map_res!(digit, from_utf8), |x: &str| {
    u64::from_str_radix(x, 10)
  }), |n: u64| {
    let mut wtr = vec!();
    wtr.write_u64::<BigEndian>(n).map(|_| Value::Number(wtr))
  })
);

// TODO add blocks
/* Parses a value given after '='. */
named!(value<&[u8], Value>, alt!(string | hex | number));

/* Parses a TLS record definition. */
/*named!(record<Record>, chain!(
                         tag!("record") ~
                         multispace ~
                         n: map_res!(alphanumeric, from_utf8) ~
                         multispace? ~ // TODO opt!()
                         tag!("=") ~
                         multispace? ~ // TODO opt!()
                         v: value,
                         || { Record::new(n.to_string(), v) }
                       ));*/

// Returns the width in bytes for a given "uint" data type.
named!(uint_width<&[u8], u8>,
  do_parse!(
    tag!("uint") >> w: map_res!(digit, |n: &[u8]| {
      match n {
        b"8" => Ok(1),
        b"16" => Ok(2),
        b"24" => Ok(3),
        b"32" => Ok(4),
        b"64" => Ok(8),
        _ => Err("unknown uint")
      }
    }) >> (w)
  )
);

// A "uint" data type with multiple accepted widths.
named!(uintx<&[u8], Type>,
  do_parse!(
    w: uint_width >> (Type::Uint(w))
  )
);

// An "opaque" data type, length prefix optional.
named!(opaque<&[u8], Type>,
  map!(alt_complete!(
    do_parse!(
      tag!("opaque(") >>
      w: delimited!(opt!(blanks), uint_width, opt!(blanks)) >>
      tag!(")") >>
      (w)
    ) | do_parse!(
      tag!("opaque") >> not!(tag!("(")) >> (0)
    )
  ), |w| { Type::Opaque(w) })
);

// A field, i.e. a type and a value.
//
// Examples:
//   uint8 name = "value";
//   opaque plaintext = { ... };
//   opaque(uint8) data = "value";
named!(field<&[u8], Field>,
  do_parse!(
    opt!(blanks) >>
    t: alt!(opaque | uintx) >>
    blanks >>
    alphanumeric >>
    opt!(blanks) >>
    tag!("=") >>
    opt!(blanks) >>
    v: value >>
    opt!(blanks) >>
    (Field::new(t, v))
  )
);

// A list of fields, separated by ";".
// The last ";" in a list can be omitted.
named!(field_list<&[u8], Vec<Field>>,
  do_parse!(
    fs: separated_list!(char!(';'), field) >>
    cond!(fs.len() > 0, char!(';')) >>
    (fs)
  )
);

// A block, i.e. a list of fields, i.e. a list of values.
//
// Examples:
//   { uint8 type = handshake }
//
//   {
//     uint8 type = handshake;
//     uint24 length = 0x0000ff;
//   }
named!(block<&[u8], Vec<Field>>,
  do_parse!(
    tag!("{") >>
    opt!(blanks) >>
    fs: field_list >>
    opt!(blanks) >>
    tag!("}") >>
    (fs)
  )
);

#[cfg(test)]
mod tests {
  use super::{block, field, hex, opaque, string, uintx};
  use super::{Field, Type, Value};
  use nom::IResult::*;
  use nom;

  /*fn parse_record(input: &str, name: &str, value: &[u8]) {
    match record(input.as_bytes()) {
      Done(_, rec) => {
        assert_eq!(rec.name(), name);
        assert_eq!(rec.value(), value);
      }
      _ => assert!(false)
    }
  }*/

  fn parse_field(input: &str, typ: Type, value: Value) {
    match field(input.as_bytes()) {
      Done(_, f) => {
        assert_eq!(f.typ(), &typ);
        assert_eq!(*f.value(), value);
      }
      _ => assert!(false)
    }
  }

  fn parse_block(input: &str, fields: Vec<Field>) {
    match block(input.as_bytes()) {
      Done(_, fs) => assert_eq!(fs, fields),
      _ => assert!(false)
    }
  }

  #[test]
  fn test_string() {
    assert_eq!(string(b"\"asdf\""), Done(&b""[..], Value::String(b"asdf".to_vec())));
    assert_eq!(string(b"\"as\ndf\""), Done(&b""[..], Value::String(b"as\ndf".to_vec())));
  }

  #[test]
  fn test_hex() {
    assert_eq!(hex(b"0xa"), Done(&b""[..], Value::Hex(b"\n".to_vec())));
    assert_eq!(hex(b"0x61"), Done(&b""[..], Value::Hex(b"a".to_vec())));
    assert_eq!(hex(b"0xa61"), Done(&b""[..], Value::Hex(b"\na".to_vec())));
    assert_eq!(hex(b"0x6161"), Done(&b""[..], Value::Hex(b"aa".to_vec())));

    assert_eq!(hex(b"0xa,0xa"), Done(&b""[..], Value::Hex(b"\n\n".to_vec())));
    assert_eq!(hex(b"0xa, 0xa"), Done(&b""[..], Value::Hex(b"\n\n".to_vec())));
    assert_eq!(hex(b"0x61,0x61"), Done(&b""[..], Value::Hex(b"aa".to_vec())));
    assert_eq!(hex(b"0x61, 0x61"), Done(&b""[..], Value::Hex(b"aa".to_vec())));
    assert_eq!(hex(b"0x61, /* foo */ 0x61"), Done(&b""[..], Value::Hex(b"aa".to_vec())));
  }

  #[test]
  fn test_uintx_success() {
    assert_eq!(uintx(b"uint8"), Done(&b""[..], Type::Uint(1)));
    assert_eq!(uintx(b"uint16"), Done(&b""[..], Type::Uint(2)));
  }

  #[test]
  fn test_uintx_failure() {
    assert_eq!(uintx(b"uint"), Incomplete(nom::Needed::Unknown));
    assert_eq!(uintx(b"uint17"), Error(nom::ErrorKind::MapRes));
  }

  #[test]
  fn test_opaque_success() {
    assert_eq!(opaque(b"opaque"), Done(&b""[..], Type::Opaque(0)));
    assert_eq!(opaque(b"opaque(uint8)"), Done(&b""[..], Type::Opaque(1)));
    assert_eq!(opaque(b"opaque(uint64)"), Done(&b""[..], Type::Opaque(8)));
  }

  #[test]
  fn test_opaque_failure() {
    assert_eq!(opaque(b"opaque(uint9)"), Error(nom::ErrorKind::Alt));
  }

  #[test]
  fn test_field() {
    parse_field("uint8 field = \"value\";", Type::Uint(1), Value::String(b"value".to_vec()));
    parse_field(" uint8 field = \"value\" ;", Type::Uint(1), Value::String(b"value".to_vec()));
    parse_field("uint8 field=\"value\";", Type::Uint(1), Value::String(b"value".to_vec()));
    parse_field("uint24 field = \"value\";", Type::Uint(3), Value::String(b"value".to_vec()));

    parse_field("opaque field = \"value\";", Type::Opaque(0), Value::String(b"value".to_vec()));
    parse_field("opaque(uint8) field = \"value\";", Type::Opaque(1), Value::String(b"value".to_vec()));
    parse_field("opaque(uint16) field=\"value\";", Type::Opaque(2), Value::String(b"value".to_vec()));

    parse_field("opaque(uint16 ) field=\"value\";", Type::Opaque(2), Value::String(b"value".to_vec()));
    parse_field("opaque( uint16 ) field=\"value\";", Type::Opaque(2), Value::String(b"value".to_vec()));
    parse_field("opaque( uint16 /* test */ ) field=\"value\";", Type::Opaque(2), Value::String(b"value".to_vec()));

    parse_field("uint8 field = 0x61;", Type::Uint(1), Value::Hex(b"a".to_vec()));
    parse_field("uint8 field = 0x6161;", Type::Uint(1), Value::Hex(b"aa".to_vec()));
    parse_field("uint8 field = 0x61, 0x61;", Type::Uint(1), Value::Hex(b"aa".to_vec()));

    parse_field("uint8 field = 97;", Type::Uint(1), Value::Number(vec!(0, 0, 0, 0, 0, 0, 0, 97)));
    parse_field("uint8 field = 097;", Type::Uint(1), Value::Number(vec!(0, 0, 0, 0, 0, 0, 0, 97)));
    parse_field("uint16 field = 24929;", Type::Uint(2), Value::Number(vec!(0, 0, 0, 0, 0, 0, 97, 97)));
  }

  #[test]
  fn test_block_success() {
    parse_block("{}", vec!());
    parse_block("{} ", vec!());
    parse_block("{ }", vec!());
    parse_block("{ \n }", vec!());

    parse_block("{uint8 field = \"value\";}",
                vec!(Field::new(Type::Uint(1), Value::String(b"value".to_vec()))));
    parse_block("{uint8 field = \"value\" ; }",
                vec!(Field::new(Type::Uint(1), Value::String(b"value".to_vec()))));
    parse_block("{uint8 field = \"value\"}",
                vec!(Field::new(Type::Uint(1), Value::String(b"value".to_vec()))));

    parse_block("{uint8 field = \"value\";uint8 field = \"value\";}",
                vec!(Field::new(Type::Uint(1), Value::String(b"value".to_vec())),
                     Field::new(Type::Uint(1), Value::String(b"value".to_vec()))));

    parse_block("{uint8 field = \"value\"; uint8 field = \"value\";}",
                vec!(Field::new(Type::Uint(1), Value::String(b"value".to_vec())),
                     Field::new(Type::Uint(1), Value::String(b"value".to_vec()))));

    parse_block("{uint8 field = \"value\"; uint8 field = \"value\"}",
                vec!(Field::new(Type::Uint(1), Value::String(b"value".to_vec())),
                     Field::new(Type::Uint(1), Value::String(b"value".to_vec()))));

    parse_block(
"{
   // here's a comment
   uint8 field = \"value\";
 }",
      vec!(
        Field::new(Type::Uint(1), Value::String(b"value".to_vec()))
      ));

    parse_block(
"{
   /* a */ uint8 /* b */ field /* c */ = /* d */ \"value\" /* e */; // f
   /*
    * uint16 field = \"value2\";
    */
 }",
      vec!(
        Field::new(Type::Uint(1), Value::String(b"value".to_vec()))
      ));

    parse_block(
"{
   // here's a comment
   uint8 field = \"value\"; // this should work
   // here's another comment
   uint16 field = \"value2\"; // this is okay too
 }",
      vec!(
        Field::new(Type::Uint(1), Value::String(b"value".to_vec())),
        Field::new(Type::Uint(2), Value::String(b"value2".to_vec()))
      ));
  }

  #[test]
  fn test_block_failure() {
    assert_eq!(block(b"{ ; }"),
               Error(nom::ErrorKind::Tag));
    assert_eq!(block(b"{ uint8 field \"value\" }"),
               Error(nom::ErrorKind::Tag));
    assert_eq!(block(b"{ uint8 field = \"value\";; }"),
               Error(nom::ErrorKind::Tag));
    assert_eq!(block(b"{ uint8 field = \"value\" uint16 field = \"value2\" }"),
               Error(nom::ErrorKind::Tag));
  }

  /*#[test]
  fn it_works() {
    parse_record("record test = \"asdf\"", "test", b"asdf");
    parse_record("record test = 0x61, 0x73, 0x64, 0x66", "test", b"asdf");
  }*/
}
