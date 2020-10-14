fn as_str<'a>(data: &'a u32) -> &str {
    let s = format!("{}", data);
    return &s;
}
fn main(){
	let x = 32;
	as_str(&x);
}	
