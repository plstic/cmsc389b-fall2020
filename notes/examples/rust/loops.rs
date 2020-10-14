
fn main(){
    
    //This is a for loop. Notie we did not have to create an I varaible.
    //ALso notice that we use an iterator like construct as 0..4 is an 
    //iterator. 
    //
    //0..4 is exclusive, while 0..=4 is iinclusive. 
    //
    for i in 0..4{
        println!("index: {}", i)
    }
    
    //Iterators
    //Supose we have a string and want to split, we do the following

    let s = "Hello World. It's very nice to meet you";
    for v in s.split(' '){
        println!("split: {}", v)
    }

    //additionally, we could make a vector or something
   let names = vec!["Hello", "World", "CMSC389B"]; 

   for name in names.iter(){
        println!("iter: {}", name)
   }

   //Now we have while loops, which take the yntacx of while _BOOLEAN_ {..}
   //
   let mut i = 0; // we need to be mut because we will be changing I.
   while i<4{
    println!("while: {}", i);
    i = i + 1;
   }

   //Lastly we have just a regular looping construct used for infiinte loops

   i = 0;
   loop{
    if i > 5 {
        break;
    }
    println!("Hello!");
    i += 1;
   }
}
