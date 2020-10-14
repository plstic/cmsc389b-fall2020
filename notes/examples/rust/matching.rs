fn main(){
    // matching acts like swith statements. Think OCaml patteern matching. Same thing
    let x = 4;

    match x{
        4 => println!("Value was 4"),
        2 => println!("Value was 2"),
        0 => println!("Value was 0"),
    }

    //We can also filter using a thing caled a guard
    //
    let tup = (,4,7);
    match tup{
        (x,y) if x == y => println!("they are the same!"),
        (x,y) if x > y => println!("{} is greater than {}", x, y),
        (x,y) => println!("{} is less than {}", x, y),
        _ => println!("Not a tuple"),
    }
}
