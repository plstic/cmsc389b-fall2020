fn main(){
    let x = String::from("Hello");
    let y = &x;
    println!("{}", y);  //this line fails. comment out to fix
    println!("{}", x);  //this line fails. comment out to fix

		// so what happens here?
		// x is the owner of some data, and x holds a reference to 
		// a string.
		// y then is given a pointer to this string, as copies are 
		// by reference if the data is on the heap.
		// y is now the owner of the the data "Hello". X looses 
		// ownership and looses acces to the value. This to help
		// avoid a double free.

}
