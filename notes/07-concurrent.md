# Concurrent Languages

Most modern languages support concurrent programming in some form.
* C uses `fork`
* Python has the `threading`, `multiprocessing`, and `subprocess` modules
* Go uses `channels`
* Rust has `std::thread` and `std::sync::mpsc` libraries
* MATLAB has the Parallel Computing Toolbox

The point here is that no language is _solely_ concurrent.
Most languages align better with other programming paradigms, **but** indeed some languages are designed with concurrency in mind.
Examples include [Go](https://golang.org/), [Alice](https://www.ps.uni-saarland.de/alice/), and [Oz](http://mozart2.org/) (I'd definitely recommend checking out Oz).

## Concurrency vs Parallelism

So what exactly is _concurrency_, and how does it differ from _parallelism_?
Excellent question!
**Concurrency** is the ability to execute multiple tasks at the same time.
**Parallelism** is simultaneous execution of multiple tasks.
They seem similar -- indeed, they are related -- but they are slightly different.
Concurrency deals with _structure_, whereas parallelism deals with _execution_.
Graphical Processing Units (GPUs) are specially designed hardware to support _parallel execution_ of tasks (eg: matrix multiplication) -- they have _multiple_ processing units ("cores") with special-access registers and memory.
On the other hand, Central Processing Units (CPUs) support 1-8 cores but are _much_ faster, and support a wider range of calculations, than GPU cores.
This works because GPUs process _graphics_, which is inherently parallel -- no two pixels on your screen are inherently dependent on each other -- whereas CPUs do more general computations which could be interdependent.
If you are looking for _parallel_ languages, then you should look for languages that interface with parallel-supported hardware (Nvidia's CUDA, AMD's FirePro, some Mainframe languages, etc) or languages that offer APIs for parallel computation (OpenMP, MPI, etc).
More info: <https://talks.golang.org/2012/waza.slide>.

As a quick side note: _automatic parallelization_ is the idea of making an executable program parallelizable, and is an unsolved, yet modern (currently being worked on), problem in computer science.
If this interests you _at all_, I'd recommend taking a compilers course.

Maybe another related resource: <https://en.wikipedia.org/wiki/Communicating_sequential_processes>.
This page relates more to how different concurrent processes communicate with each other, yet is still tangentially related to automatic parallelization.
Interested in that?
Do a search for "process calculi" -- a family of formal systems for describing/modeling concurrency.
Pretty cool stuff.

## Go

There's no project for this language -- we don't expect you to learn this language.
Not that this course expects you to "learn" any of the languages anyway -- but regardless.
We still provide a small introduction, along with an abundance of resources.
Go -- or, Golang -- was created at Google when Google engineers were essentially like

> hey bro, current languages suck because they don't handle concurrency nice and safely or whatever

and then the other engineers were like

> bro just make your own language bro

and so Robert Griesemer, Rob Pike, and Ken Thompson set out in the late 2000s to create the best friggin language to-date.

Okay -- it's not perfect.
It's pretty darn good though.
For more higher-level, yet still pretty low-level, software problems, Go solves them pretty well.
I've personally written file systems and databases in Go for a graduate class.
It's awesome.
Most of the Twitch back-end is written in Go <https://blog.twitch.tv/en/tags/engineering>.
Most of Google's back-end is written in Go (obviously).
<https://brainhub.eu/blog/biggest-companies-using-golang/>.
Just search for engineering blogs for any service you're a fan of -- they probably use some form of service-oriented architecture (SOA) where some services are written in Go.

Check out their webpage: <https://www.golang.org/>.
Then, check out their tour: <https://tour.golang.org/welcome/1>.
It's pretty good.

### Basics

_Go_ here for a fast-ish tutorial with code examples: <https://golang.org/doc/effective_go.html>.

Again -- this class is not to teach you a language, moreso to learn how to "pick up" a language.
We want to teach you concepts, such that you just need to look at syntax and some examples.

Imagine Python meets C -- that's Go.
If you ever need to search for how to do something in Go, reference "golang" in your query.

For this section, we're just going to explain the basic concepts along with the important concurrency concepts.

Go uses tabs -- you can type using spaces or whatever, but the Go linter will format your code to use tabs.
This is clearly a design flaw.

To start -- Go requires a starting point for everything.
This is the main function.
This is also typically (possibly required to be) placed in the main package.

Go has packages -- they can be simple libraries, or they can be the starting point for your codebase.

```go
package main // name the current package

import (
	"fmt" // import the format package
)

func main() {
	// simple print-line statement
	fmt.Println("Hello World!")
}
```
This prints `Hello World!`.
Comments are written using `//`, and block comments are written with `/* comment */`.
Note that Go is a compiled language, but it has a nice `go run` utility that will compile and execute for you.
You might run the above code as `go run hello.go` if the filename is `hello.go`.

Go is statically typed, meaning your variables have to have an associated type during declaration and cannot change types.
Go has primitives like numeric types (`uint8 -- uint64`, `int8 -- int64`, `float32`, `float64`, `complex64`, `complex128`), `bool`, `string`, and `error`.
Go also has complex types like `chan` (channels), `map` (hashtables), and slices (arrays) -- these each hold values taking a specific type.
You typically need to `make` or `new` these types of variables.
Go also has `struct` structures (just organized data -- think of it like an object).
Variables are declared like so: `varName varType` (read as "varName is a varType" -- eg `var test string` says that "test is a string type").
If you're setting a variable, and the type can be inferred, then you can assign the variable using `:=` -- otherwise just use `=` for assignment.
```go
package main

import "fmt"

func main() {
	primes := [6]int{2, 3, 5, 7, 11, 13}
	fmt.Println(primes)
	var s []int = primes[1:4]
	fmt.Println(s)

	hashes := map[int]string{}
	for p := range primes {
		hashes[p] = "value"
	}
	fmt.Println(hashes)

	for key, val := range hashes {
		if key == 5 {
			fmt.Println("key == 5")
		} else {
			fmt.Println(key, val)
		}
	}
}
```
Go also has pointers, but de/referencing is sometimes nicely handled for you.

One sticking point in Go is that you should never declare something and **not use it**.
Any imported, but not used, packages will be deleted by the linter.
Any unused variables will cause a warning/error.
Pretty cool stuff.

#### Goroutines

Goroutines are sets of executable code that can be executed on another thread.
This execution happens concurrently, unless your CPU has multiple cores.
```go
package main

import (
	"fmt"
	"time"
)

func main() {
	for i := 0; i < 5; i++ {
		go func() { // this is a goroutine
			fmt.Printf("I am goroutine %v!\n", i)
			time.Sleep(5 * time.Second)
			fmt.Printf("%v is done now!\n", i)
		}() // the () here says to execute it
	}
}
```
You'll notice that this doesn't print anything though!
That's because the `main()` function ends before the goroutines can do anything!
We can fix this by blocking until all of our routines have ended.
We canonically solve this using channels.

#### Channels

Channels are how you communicate between threads.
`<-channel` reads a value out of a channel, and `channel <- value` puts a value into a channel.
Notice the direction is constant, whereas the channel location changes.
Reading from empty channels causes current execution to _wait_ until it receives a value.
Let's use this to our advantage and fix the previous code.
```go
package main

import (
	"fmt"
	"time"
)

func main() {
	var tot int = 5
	var c chan int = make(chan int, tot)
	for i := 0; i < tot; i++ {
		go func() { // this is a goroutine
			fmt.Printf("I am goroutine %v!\n", i)
			time.Sleep(5 * time.Second)
			fmt.Printf("%v is done now!\n", i)
			c <- 1
		}() // the () here says to execute it
	}
	for i := 0; i < tot; i++ {
		<-c  // we can simply read the value to block the main thread
		// but notice that waiting for the 0th thread
		//  does not affect the _execution_ of the others
		//  since we're waiting _on the main thread_
	}
	fmt.Println("all done!")
}
```
Note that [WaitGroups](https://golang.org/pkg/sync/#WaitGroup) solve this problem, but likely use the same implementation.

Now this runs, but it only displays `i=5` for every goroutine!
This shows that goroutines share state.
We fix this by shadowing an argument in the goroutine function.
```go
// ...
		go func(i int) { // this i references the argument input
			fmt.Printf("I am goroutine %v!\n", i) // this i == func arg
			time.Sleep(5 * time.Second)
			fmt.Printf("%v is done now!\n", i) // this i == func arg
			c <- 1
		}(i) // this i references the for-loop
// ...
```

Now we get some nice output:
```txt
I am goroutine 4!
I am goroutine 0!
I am goroutine 2!
I am goroutine 1!
I am goroutine 3!
4 is done now!
0 is done now!
2 is done now!
1 is done now!
3 is done now!
all done!
```
Of course, this will necessarily look different on any subsequent execution.
There are minimal/no ordering guarantees placed on channel creation and execution.

### Examples

First, check out these resources:
* <https://www.golang.org/>
* <https://tour.golang.org/welcome/1>
* <https://golang.org/doc/>
* <https://www.tutorialspoint.com/go/index.htm>

Then check out the basic server example in the examples folder.
There's a README that explains how to execute it.
Feel free to play around with it.
