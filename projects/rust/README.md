# Security

Due: 25 October 2020, 11:59pm EDT  

Points: 30 Public, 70 semipublic 

## Before You Start

Please make sure that if you are not using docker (or have updated things on the
docker image), that you are using the at least `rustc 1.46.0`. You 
can check this by running `rustc --version`. If the command does not run, then 
you do not have Rust. 

### Introduction

This project has two functions, you need to fill out, and one segment of code 
you need to complete: `string_to_integer`, `integer_to_string` and part of the 
`main` function.

#### string_to_integer

This function will take in a reference to a string slice, and return an integer.
The string that is being referenced consists of the written version of the digits
that make up the integer. 

For example, the string "one two three" should have the equivalent integer of 
123. The string of "four four six seven" should have the equivalent integer of
4467. You will take in a reference to such a string, and return the equivalent 
integer. I should suggest finding a way to isolate the intevidual digit strings,
and associating them with the digit and then doing some fancy math to combine
them.

One thing to note: the string "zero" and the string "oh" should both be associated 
with the 0 digit. So "two oh two zero" should be 2020. Also, the input will all
be lowercase.

#### integer_to_string 

This function will take in an integer, and then return a string of the digits
written out. This is the inverse of the previous function. So if the input is
123, the returned value should be "one two three". If the input is 4467, the 
returned string should be "four four six seven". I would suggest using some 
fancy modulous math to get each digit and build the string that way. 

One thing to note: the digit 0 should be associated with "zero". There should
be no output with "oh". Additionally, the string must be all lowercase. 

Thus, `string_to_integer(integer_to_string(123))` should return 123.

#### standard IO

The last part of this project involves reading a properly formated string from 
stdin. I will handle the rest of calling the functions with input and printing
out what needs to be printed out.

### Testing and Submitting

You can run the tests with `cargo test`. This is Rust's testing framework. 
You just need to submit `project.rs`. 

+ 30% Public Tests
+ 70% semipublic Tests

The code for the public tests are given to you. When you submit to gradescope,
you will be given the names of the semiublic tests and told if you fail or pass
them.
