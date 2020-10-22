package main

import (
	"fmt"
	"time"
)

func main() {
	var tot int = 5
	var c chan int = make(chan int, tot)
	for i := 0; i < tot; i++ {
		go func(i int) { // this is a goroutine
			for j := 0; j < 3; j++ {
				fmt.Printf("   %v - %v\n", i, j)
				time.Sleep(1 * time.Second)
			}
			c <- 1
		}(i) // the () here says to execute it
	}
	for i := 0; i < tot; i++ {
		<-c  // we can simply read the value to block the main thread
		// but notice that waiting for the 0th thread
		//  does not affect the _execution_ of the others
		//  since we're waiting _on the main thread_
	}
	fmt.Println("all done!")
}

// two separate execution runs:
/*
   4 - 0
   0 - 0
   1 - 0
   2 - 0
   3 - 0
   4 - 1
   0 - 1
   1 - 1
   2 - 1
   3 - 1
   4 - 2
   0 - 2
   1 - 2
   2 - 2
   3 - 2
all done!
*/

/*
   4 - 0
   1 - 0
   0 - 0
   2 - 0
   3 - 0
   2 - 1
   4 - 1
   1 - 1
   0 - 1
   3 - 1
   2 - 2
   4 - 2
   1 - 2
   3 - 2
   0 - 2
all done!
*/
