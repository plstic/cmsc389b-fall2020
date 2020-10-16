extern crate rust_project;

use rust_project::project::{string_to_integer,integer_to_string};

#[test]
fn public_test_to_int() {
    assert_eq!(132, string_to_integer("one three two"));
    assert_eq!(62, string_to_integer("six two"));
}

#[test]
fn public_test_to_string() {
    assert_eq!("one three two", integer_to_string(132));
    assert_eq!("nine two three", integer_to_string(923));
}

