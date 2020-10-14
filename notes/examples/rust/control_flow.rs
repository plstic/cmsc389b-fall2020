fn main(){
    // very typical if-elseastatements. We dont need to put the boolean
    // in parenthesises though.
    //
    if 3 > 2 {
        println!("Branch1A");
    } else if 4 > 3 {
        println!("Branch2A");
    } else {
        println!("Branch3A");
    }

    if 2 > 2 {
        println!("Branch1B");
    } else if 4 > 3 {
        println!("Branch2B");
    } else {
        println!("Branch3B");
    }

    if 2 > 2 {
        println!("Branch1C");
    } else if 4 > 5 {
        println!("Branch2C");
    } else {
        println!("Branch3C");
    }
}
