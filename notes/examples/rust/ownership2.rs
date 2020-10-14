fn main(){
	let mut s1 = String::from("hello");
	{ let s2 = &mut s1;
		s2.push_str("world!\n"); //disallowed
	} //drops s2
	s1.push_str("world!"); //ok
	println!("String is {}",s1);//prints updated s1

	let x = 32;
	x = 22;
}
