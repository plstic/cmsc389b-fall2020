fn main(){
	let mut s1 = String::from("hello");
	let s2 = &s1;
	s1.push_str("World");
	println!("{}", s2);
}
