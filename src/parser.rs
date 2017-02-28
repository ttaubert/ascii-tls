use std::iter::FromIterator;
use std::str::from_utf8;

use nom::{alphanumeric, digit, eol, multispace, not_line_ending, space};

use types::{Field, Record, Type};

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
named!(string<&[u8], Vec<u8>>,
       map!(delimited!(char!('"'), is_not!("\""), char!('"')), |s: &[u8]| { s.to_vec() }));

// TODO
// separated_nonempty_list(sep, X) returns a Vec<X> of at list one element
// preceded!(opening, X) returns x
/*named!(hex<&[u8], Vec<u8>>, many1!(map_res!(chain!(tag!("0x")? ~ v: take!(2) ~ tag!(",")?, || { v }), |bytes: &[u8]| -> Result<u8, &str> {
  fn convert(byte: u8) -> Result<u8, &'static str> {
    match byte {
      b'a'...b'f' => Ok(10 + byte - b'a'),
      b'A'...b'F' => Ok(10 + byte - b'A'),
      b'0'...b'9' => Ok(byte - b'0'),
      _ => Err("invalid hex")
    }
  }

  assert!(bytes.len() == 2);

  println!("byte...");
  convert(bytes[0]).and_then(|hi| {
    convert(bytes[1]).and_then(|lo| {
      println!("byte: {}", hi * 16 + lo);
      Ok(hi * 16 + lo)
    })
  })
})));*/

/* Parses a value given after '='. */
named!(value<&[u8], Vec<u8>>, alt!(string));

/* Parses a TLS record definition. */
named!(record<Record>, chain!(
                         tag!("record") ~
                         multispace ~
                         n: map_res!(alphanumeric, from_utf8) ~
                         multispace? ~ // TODO opt!()
                         tag!("=") ~
                         multispace? ~ // TODO opt!()
                         v: value,
                         || { Record::new(n.to_string(), v) }
                       ));

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
      tag!(")") >> (w)
    ) | do_parse!(
      tag!("opaque") >> not!(tag!("(")) >> (0)
    )
  ), |w| { Type::Opaque(w) })
);

// TODO
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
    tag!(";") >>
    opt!(blanks) >>
    (Field::new(t, v))
  )
);

// TODO
named!(block<&[u8], Vec<Field>>,
  do_parse!(
    tag!("{") >>
    opt!(blanks) >>
    fs: many0!(field) >>
    opt!(blanks) >>
    tag!("}") >>
    (fs)
  )
);

#[cfg(test)]
mod tests {
  use super::{block, field, record, string, opaque, uintx};
  use super::{Field, Type};
  use nom::IResult::*;
  use nom;

  fn parse_record(input: &str, name: &str, value: &[u8]) {
    match record(input.as_bytes()) {
      Done(_, rec) => {
        assert_eq!(rec.name(), name);
        assert_eq!(rec.value(), value);
      }
      _ => assert!(false)
    }
  }

  fn parse_field(input: &str, typ: Type, value: &[u8]) {
    match field(input.as_bytes()) {
      Done(_, f) => {
        assert_eq!(f.typ(), &typ);
        assert_eq!(f.value(), value);
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
    parse_field("uint8 field = \"value\";", Type::Uint(1), b"value");
    parse_field(" uint8 field = \"value\" ;", Type::Uint(1), b"value");
    parse_field("uint8 field=\"value\";", Type::Uint(1), b"value");
    parse_field("uint24 field = \"value\";", Type::Uint(3), b"value");

    parse_field("opaque field = \"value\";", Type::Opaque(0), b"value");
    parse_field("opaque(uint8) field = \"value\";", Type::Opaque(1), b"value");
    parse_field("opaque(uint16) field=\"value\";", Type::Opaque(2), b"value");

    parse_field("opaque(uint16 ) field=\"value\";", Type::Opaque(2), b"value");
    parse_field("opaque( uint16 ) field=\"value\";", Type::Opaque(2), b"value");
    parse_field("opaque( uint16 /* test */ ) field=\"value\";", Type::Opaque(2), b"value");
  }

  #[test]
  fn test_block_success() {
    parse_block("{}", vec!());
    parse_block("{} ", vec!());
    parse_block("{ }", vec!());
    parse_block("{ \n }", vec!());

    parse_block("{uint8 field = \"value\";}",
                vec!(Field::new(Type::Uint(1), b"value".to_vec())));

    parse_block("{uint8 field = \"value\";uint8 field = \"value\";}",
                vec!(Field::new(Type::Uint(1), b"value".to_vec()),
                     Field::new(Type::Uint(1), b"value".to_vec())));

    parse_block("{uint8 field = \"value\"; uint8 field = \"value\";}",
                vec!(Field::new(Type::Uint(1), b"value".to_vec()),
                     Field::new(Type::Uint(1), b"value".to_vec())));

    parse_block(
"{
   // here's a comment
   uint8 field = \"value\";
 }",
      vec!(
        Field::new(Type::Uint(1), b"value".to_vec())
      ));

    parse_block(
"{
   /* a */ uint8 /* b */ field /* c */ = /* d */ \"value\" /* e */; // f
   /*
    * uint16 field = \"value2\";
    */
 }",
      vec!(
        Field::new(Type::Uint(1), b"value".to_vec())
      ));

    parse_block(
"{
   // here's a comment
   uint8 field = \"value\"; // this should work
   // here's another comment
   uint16 field = \"value2\"; // this is okay too
 }",
      vec!(
        Field::new(Type::Uint(1), b"value".to_vec()),
        Field::new(Type::Uint(2), b"value2".to_vec())
      ));
  }

  #[test]
  fn test_block_failure() {
    assert_eq!(block(b"{ uint8 field \"value\" }"),
               Error(nom::ErrorKind::Tag));
    assert_eq!(block(b"{ uint8 field = \"value\" uint16 field = \"value2\" }"),
               Error(nom::ErrorKind::Tag));
  }

  #[test]
  fn it_works() {
    assert_eq!(string(b"\"asdf\""), Done(&b""[..], b"asdf".to_vec()));
    //assert_eq!(hex(b"0x61,0x73,0x64,0x66"), Done(&b""[..], b"asdf".to_vec()));

    //parse_record("record test = \"asdf\"", "test", b"asdf");
    //parse_record("record test = 0x61, 0x73, 0x64, 0x66", "test", b"asdf");
  }
}
