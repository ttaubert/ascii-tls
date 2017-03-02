#![crate_name = "ascii_tls"]
#![crate_type = "lib"]

#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]

#[macro_use]
extern crate nom;
extern crate byteorder;

mod parser;
mod types;
