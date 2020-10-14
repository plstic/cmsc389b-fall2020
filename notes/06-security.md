Cliff
# Security in languages

I know what you're thinking. "Rust? But we already covered Rust in 330. Why 
don't we learn a new language?" To that I say, We are covering languages not 
because they are new, but because they have something interesting about them. 
As a 330 TA I know you covered Rust, but I also know which parts of Rust you 
covered. 330 focused on the Ownership, borrowing and the mixed syntax. Rust 
however doesn't just have these constructs because they felt like it. Rather, 
Rust was designed to incorporate an aspect of software engineering that many 
people overlook: security. 

## What does Security mean?

We hear in the news that Twitter got hacked. We hear that people's social 
security numbers got leaked. We hear when Facebook is down. What exactly does 
this entail? When an attack happens, I would say there are three things this 
could mean.

  + Attack 1: data which should not be read was read
  + Attack 2: data which should not be modified was modified
  + Attack 3: data which should be public was blocked.

Out of these attacks, most of these attacks are due to memory misuse (danging 
pointers, memory leak, old pointer access, multithreading problems). Security 
is merely the means to prevent these three things from happening (and proving 
it).

## What makes a language secure?

Let us remember C, this a low level language with pointers and very little 
checking of security. There is not ArrayIndexOutOfBounds exception like we may 
see in java. If something is null, and you don't mean it to be, it will usually
crash or continue. Any security that you want in a C application has to be built
in the program. CMSC414 goes more into this, but for now I'll just say that 
security is important, and must be considered when building a program, not as a
later security patch. Sure, as you go towards more higher level languages like
java or swift, you get this security just due to how abstracted you are away 
from assembly. Rust is unique because it can give you the control you like with
lower level languages, but also has the security we are looking for.

## Attacks

So for more in depth section about attacks see the bottom of the notes, but for
the purpose of this course, I'll give a very brief explanation of attacks or 
vulnerabilities that can	occur in C that Rust mitigates.

### Buffer overflow

Let us consider the following C code:

```c
char buffer[5];
scanf("%s", buffer);
```

If the input is something like "hackerman", then data past the end of the buffer
will get overwritten. Depending on where `buffer` exists in memory, this could
be devastating. Now, there are alternative functions like `fgets()` which 
account for buffer size, or you could even put `"%4s"` as the first argument to 
`scanf` to ensure no data is being overwritten. This is known as a 
[buffer overflow](https://en.wikipedia.org/wiki/Buffer_overflow) attack.
Rust can help mitigate this with	the use of splices and vectors.

### Memory 

Let us consider the following C code:

```C
struct Node{
	int value;
	struct Node* next;
}

struct Node *head = malloc(sizeof(struct Node);
struct Node *tail= malloc(sizeof(struct Node);
head->value = 10;
tail->value = 14;

head->next = tail;
free(tail);
if (head->next == NULL){
	do_some_task();
}
```

This is considered a dangling pointer and finding situations where this happens
can be very difficult when debugging. Many vulnerabilities that exist can be 
traced back to some sort of memory issue like this. Rust can help prevent, but
not fully stop vulnerabilities like this from occurring, through a combination of
lifetimes and ownership.

### Time Attacks

Let us consider the following C code:

```c
if (access("/home/student/file.txt", R_OK) != 0) {
	 exit(1);
}
//comment 1
fd = open("/home/student/file.txt", R_WRONLY);
read(fd, buffer, 1023);
```

Do you remember way back when to 216 when we talked about multithreading? The 
idea is that the computer switches between tasks and execution could pause at any
point. So what if I had the following C code running as a different process?

```c
symlink("/secret/root/file", "/home/student/file.txt");
```

If the first segment of code had execution paused where ``//comment 1` was, then
the second program ran, then `file.txt` now points to a different file. 
Potentially a password file or something with sensitive information. Then when 
the first program resumes execution, it will open the now symbolicly linked 
file and permissions do not matter. This is because the time of checking 
permissions is different than the time of actually opening the file. This is 
called a [time-of-check, time-of-use](https://en.wikipedia.org/wiki/Time-of-check_to_time-of-use)
vulnerability/attack. This can be mitigated by Rust due to the ownership and 
borrowing rules that exist in Rust. 

## Rust

Rust was created with security in mind, with parts of the language making it 
theoretically memory safe. This is all thanks to Rust's natual immutable state,
its concepts of ownership, borrowing, and liftimes. I think, that once you get a
good understanding of what is happening in memory, and what pointers are, 
everything else falls into place. Hopefully these notes help you understand what
exactly memory looks like. 

### Ownership and Borrowing

Each piece of data in Rust has both an owner and a lifetime. The owner of data
is the only entity that can change said data. The lifetime determines where the
piece of data can be seen, or how long it exists in memory. Usually the lifetime
is dependent on if the owner is in scope or not (although this can be 
overwritten). Ownership can be transfered if needed. 

Yet what does ownership mean? Let us consider what we learned in 216. I find 
that if we act like boats and stay at C level, we get a good understanding of
most things.

When we declare a primitive, something like `int a`, we are telling the compiler
to allocate and reserve an int's size of data (`sizeof(int)`) on the stack.
Data on the stack is addeded continously, so if I declare `int a; int b`, the 
places on the stack where I am to store the values of `a` and `b` are next to 
each other. This is no different when I declare a pointer, something like 
`char *buffer`, we are telling the compiler to allocate a pointer's size of data 
on the stack. Addionally, the computer keeps track of the memory address on the 
stack and associates a varialbe name for it. That is all that is happening.
This is pretty boring in my opinion, but it's crucial to understand what is 
happening.

It is when when assign the data a value do things get really interesting. That 
is, when I say something like `a = 32`, then I am placing the value of 32 into 
the segment of memory on the stack the computer associates with the varaible 
name `a`. Yet then suppose, that I was playing around with pointers. When I say
something like `buffer = malloc(sizeof(char) * 80)`, then I am telling the 
computer to allocate space, not on the stack, but on the heap. The starting 
memory address of memory allocated is then placed on the stack where the space
that is asscoiated with `buffer` is. Then when I say something like 
`strcpy(buffer, "hello, world");` am I placing values into the place that has
been allocated on the heap.

In Rust, when we speak of ownership, we mean which variable has the privledge
of pointing to the data that it associated with its memory address. When we 
speak of a reference, or borrowing, we mean that this privlege is temporarily
given to another variable, and then eventually, the privledge is revoked and
the original varaible reclaims owenership. 

It is important to note that ownership and references are by default, immutable.
Should you want the owner to have the ability to change it's value, 
you must use the `mut` modifier. Consider the following pieces of code:

```Rust
let x = 32;
x = 22; //not okay
------------------
let mut x = 32;
x = 23; //okay
```


To create a reference you can use the `&` symbol. This is like creating a 
pointer with `*` in C. To create a mutable refernce you use `&mut`. Remember 
that heap data is copied by refernce, while primitives are copied by value. 

So consider the following segments of code:
```Rust
let x = String::from("hello");
let y = x; //owernship transfered
println!("{}!", x) //not okay
// one piece of memory is allocated and y points to it. Once x lost ownership
// then it is 'freed'
-----------------------------
let x = 42;
let y = x; //deepcopy made (cloning) so ownership is not ransfered
println!("{}", x) //okay
// two pieces of memory are allocated and y and x are independent
----------------------------
let x = 42;
let y = &x; //y is given  immutable reference to y.
println!("{}", x) //okay
// one piece of memory is allocated and both x and y point to it. X is the 
// owner though.
```

The intersting part is how mutable ownership and mutalbe references work. 
A reference 'inhereits' the abilities of the original, so should the original
value be considered immutable, then the borrowed reference cannot modify
the data. That is, you cannot make a mutable refernce from an immutable piece
of data.

Consider the following: 
```Rust
let mut s1 = String::from("hello");
let s2 = &mut s1; 
// s1 is the mutable owner, then s2 is given a mutable reference.
//both s1 and s2 can modify data as long as s2 is not borrowed.
------------------------------------
let s1 = String::from("hello");
let s2 = &mut s1; //not allowed as s1 is not mutable
------------------------------------
let mut s1 = String::from("hello");
let s2 = &s1; 
// s1 is mutable owner, s2 is immutable reference, s1 con still modify
// but only as long as s2 is not modified.
```

### Lifetimes --not in scope of this class

These are rules that Rust has that determines how long a reference is valid for.
This is how control of ownership is passed around. For this class, lifetimes 
will coicide with scope as scope usually determines how long a variable lives.

Consider the following:
```Rust
fn myfunc(data){
    let s = format!("{}", data);
    return &s
}
```
Here we are trying to return a reference to local varaible that will die when 
the function returns. 

### Slices 

Remember when we talked about how things look in memory? Well, slices and 
vectors tell us where memory is held, and something about that memory. In 
otherwords, slices and vectors contian metadata about the data they are refering
to. Let us consider the string slice. A string, in C, is a character array.
Suppose I have a 6 character array `['h','e','l','l','o','\0']`. A slice, is a 
segment of this array, with a start and ending index. 

So consider the following:
```Rust
let mystr = "Hello World".to_string();
let myslice = &mystr[4..9]; 
//mysplice is "o Wor" and notice that it must be a reference.
```

You can create mutable slices, as long as the original data is mutable.
```Rust
let mut mystr = "Hello World".to_string();
let myslice = &mut mystr[4..9];  // this is fine
--------------------------------------
let mystr = "Hello World".to_string();
let myslice = &mut mystr[4..9]; //this is less fine
```

### Applications

Okay sure, this is all well and good, and to be honest, a little confusing and
maybe a tad boring. Yet, lets think about what we just learned and apply it to
the attacks I described earlier. Even better, let's talk about ownership, 
borrowing and pointers in terms of attacks.

When a buffer overflw attack occurs, it is usually because the size of a buffer
is not considered. In C this can usually befixed by used `strncpy` or something
similar. At the very least, by doing some sorts of bounds checking. In Rust, due
to smart pointers containing meta data like the size of a buffer, this check is
not usually necessary as Rust will do it for you. Additionally, remember that
all data is immutable, so if anything, an error will be raised, as a slice 
cannot be longer than the original. 

On the other hand, memory attacks can occur for a variety of reasons, from not
NULLing a pointer, for creating race conditions, or using pointer arithmetic to
access data that should otherwise be hidden. By including an ownership and 
borrowing system, Rust has emliminated the possibility of a race condition, 
(in your program, assuming you are using safe Rust). Now technically, computers
in general use multithreading and can get into data races. Your OS also probably
gets in a plethora of data races all the time. So sure, Rust programs can go 
into deadlock or do something bad. However, because only one owner or mutable 
reference can exist at a time, memory safety is perserved. So if we consider
the previous TICTOU example:
```C
		if (access("file", R_OK) != 0) {
			 exit(1);
		}
		//comment 1
		fd = open("file", R_WRONLY);
		read(fd, buffer, 1023);
```
Once `"file"` is defined in the rust program, nothing can change the state of 
the	program except the owner. So attacker's code or interference cannot change 
where `"file"` points to in the scope of the program. The state of the machine
is preserved and thus somewhat "sandboxed" away from any changes that could have
occured.

## More in depth sections that I cut out.

#### Buffer Overflow Attack

Buffer overflow attacks have to deal usually with how a stackframe is created
and placed on a stack. We can remember from assembly that we have a few parts
of a stack frame. We have the function parameters, a place to store where we 
return to, and we need stack pointer. A bufferoverflow attack usually tagets
these values. But in this class we won't go that deep. I also find it easier 
to talk about this using an example. 

Let's start by considering the buffer overflow attack that vuln.x has. A buffer
overflow attack is when data is written past a buffer's defined bounds. In a
language like C, this is pretty easy because you can just write data to memeory
thanks to pointer arithmetic. Take for example, how strings are structed in C.
There is a pointer to segment in memory. The programmer then decides to treat
each byte starting at that address as a character and reads/writes until the
null character (`\0`) is read. However with functions like `printf` and `scanf`
there is no bounds checking which are a part of these functions.

The vuln.c program uses something similar thus if a malicius user attemts to
input a very long password, other program data can be overwritten and thus the
program could either crash, or print "Accepted." If this program was a website,
if you crashed a website you could potentially perform aforementioned attack 3.
If you logged in to an admin account on a data server or something similar, you
could potentially perform attack 1 or 2. 

If you want to get even deeper into a full attack, consider how a stack frame
is created. When a stack frame is created, there is a piece of data that tells
the computer where to return to. If you could overwite this return pointer, you
could have it point to a malicious piece of code you wrote, or even perform a 
[ROP](https://en.wikipedia.org/wiki/Return-oriented_programming) attack. If 
you're more interested in this type of thing, you should take CMSC414.

Anyway, while there are built in functions like `fread` which you can then limit 
how much data can be written or read, Rust will do this automatically meaning 
that it is very difficult to be susceptible to such an attack. Again, if you 
care about security, you should incorporate it as you build your program, not
afterwards. Might as well use a language with that philosophy to help you.

### Memory 

I once had a bug very similar to this in one of my 420 projects. Something like
this could be paired up with buffer overflow attack or even using a printf 
incorrectly. Did you guys know that something like `printf("%n") as a format
specifier will actually write data? Makes no sense. Anyway, let us consider the 
following C code:

```C
struct Node{
	int value = 0;
	struct Node* next = NULL;
}

struct Node *head = malloc(sizeof(struct Node);
struct Node *tail	= malloc(sizeof(struct Node);
head->value = 10;
tail->value = 14;

head->next = tail;
free(tail);
tail = NULL;
tail = malloc(sizeof(struct Node);
//comment 1
if (head->next->value != 10)
	do_something();
```

Now the bug I was getting was an odd one. `do_something()` was being called. 
After hours of using GDB , I figured out exactly what was happening, I 
understood my mistake. Recall how ways that `malloc` worked. In 216, we talked
about first and best fit. Also remember that when we call `free`, the data is
not actually erased, but rather, a bit if flipped to tell the computer if the
segment of memory is free or in use. Under very perticular conditions, I could
free a segment of memory, call malloc, and get back the very original segment of
memory. For anyone wondering, the solution was to initialize the struct's 
fields after malloc'ing. So `tail->next = NULL; tail->value = 0;` where 
`//comment 1` was. 

Dangling pointers similar, or even the same as this example can be a security
concern. Rust can decrease the probability of this occuring with the ownership
rules that exist. 

#### Time Attacks
As we just saw, it canbe frustrating when it comes to C and memory leaks.
The constant balancing act of `malloc`-ing and `free`-ing can be tiring.
Additionaly pointer arithmetic and making sure you looking at the value the
pointer points to rather than the pointer address can be hard to keep track of.
This can be compounded when we talk about pointers to pointers, and pointers to
pointers to pointers.

It is due to the complicated nature of pointers that many people don't even want
to consider security when it comes to pointers. Yet, as a C or similar developer,
when you consider security, you must think of not just how your program should
work, but rather, what happens *over time*. The state of a computer is usually
always changing during the lifetime of a program. Due to the nature of pointers,
you must consider how the state changes. If I have a pointer to a secure file
during a `root` call but hold onto that pointer when I no longer have root acces,
I can still read that file. Consider the following code:

```c
	char buffer[1028];
	int id = fork();
	if (id > 1){
		if (access("file", R_OK) != 0) {
			 printf("cannot access file \n")
			 exit(1);
		}
		//comment 1
		fd = open("file", R_WRONLY);
		read(fd, buffer, 1023);
	}
```

This is an example of a time-of-check time-of-use vulnerability.
When we multithread, there is no garuntee when a process will be run or stopped.
Suppose that "file" is something you can read. Then the process is put on hold
at comment 1's position. Suppose the computer just performed another task:
`symlink("/etc/passwd", "file");`. Then when I go to open the file, I am able to
read the password file. Granted, I don't even need to fork to potentially do
this, but the fork does make it easier, as well as more plausible. Web servers
multithread most of thier work to deal with potentially thousands of users at a
time.

Due to Rust's built in system, there will never be a data race in Rust should
you use Rust correctly. This means this type of attack has a decreased chance of
occuring. But even in Rust, don't do this, as timeing attacks like this still
have a chance of occuring.

