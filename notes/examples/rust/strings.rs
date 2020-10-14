//String is a mutable owned value
//&str is a reference, a splice/vector of a String.

fn main() -> io::Result<()>{
	let owned_string: String = "hello ".to_owned();
	let borrowed_string: &str = "world";

	let new_owned_string = owned_string + borrowed_string;
	println!("{}", new_owned_string);
}
