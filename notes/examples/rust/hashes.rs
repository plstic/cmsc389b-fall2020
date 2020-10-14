use std::io;
use std::collections::HashMap;

fn main() -> io::Result<()> {
	let mut convert = HashMap::new();
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

	convert.insert("key1","Value1");
	convert.insert("key2","Value2");
	convert.insert("key3","Value3");
	convert.insert("key4","Value4");
	convert.insert("key5","Value5");
	match convert.get(&input.trim()){
		Some(&value) => println!("{} was found",value),
		_ => println!("Value not found"),
	}
  Ok(())
}
