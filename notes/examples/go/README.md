# Go

## How to run the thing

```txt
$ go run server.go [port, default=80]
```
if you're using the docker image, use port `65001`.

Then you can visit <http://127.0.0.1:65001/> in your browser.
You can go to `/put` and use query parameters to put a key/val pair into a hashmap `/put?name=test&val=1`.
You can only have string keys mapped to integer values.
Putting a value returns to you a `job_id` -- the key/val is not immediately placed into the hashmap.
You can check `/job` and use query parameters to view a job status `/job?id=LONG_HASH`.
You can go to `/get` and use query parameters to get a value from the hashmap using the key `/get?name=test`.
You can go to `/end` to close the server.
Ending will display the entire hashmap on your browser.

```txt
/put?name=NAME&val=VAL ->
  succ: {job_id: "LONG_HASH"}
  fail: "not found" or "NAME not found"

/job?id=LONG_HASH ->
  succ: {job_id: "LONG_HASH", job_type: put/get/end, job_status: queued/started/finished/error, name: ..., value: ...}
  fail: "not found" or "LONG_HASH not found"

/get?name=NAME ->
  succ: "NAME: VAL"
  fail: "not found" or "NAME not found"
```

Values are not immediately placed into the hashmap -- instead, a 5-second wait is introduced.
Presumably, this could be a super long image-processing or neural-network calculation.
You'll notice, though, that the server is still responsive!
This is because the long calculation is spawned in a goroutine, and executed concurrently.

To see this:
1. paste <http://127.0.0.1:65001/put?name=zero&val=0> and <http://127.0.0.1:65001/get?name=zero> in two separate tab address bars
1. press enter on the first one
1. switch to the second one, press enter, and constantly refresh
1. eventually, the second one will stop displaying "not found" and instead show "zero: 0"

Cool, right?!

Feel free to play with this code :D

It's not perfect.
There could be race conditions here -- consider if two people write to the hashmap at the same time.
Who should win, and why?

## VSCode

If interested: [extension](https://marketplace.visualstudio.com/items?itemName=golang.Go)

You'll need a Go executable in your system path for this extension to work, though.

This extension also runs the Go linter every time you save to make your code look pretty!
