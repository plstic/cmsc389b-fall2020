# J

## Preliminaries

J scripts have the extension `*.ijs`.

We are using J version `9.01`.

This assignment is due **Sunday, October 18th at 11:59pm on Gradescope**.
You will submit one `hw.ijs` to the `J` Gradescope assignment.
You will be graded on public tests -- make sure you're passing each test locally before you submit.

### Main Console

You can run `jconsole` or `ijconsole` in your container to bring up a command-line terminal/REPL.
You can/will also use this to execute your J scripts.
Use `ctrl+d` to exit.

### Web Console

**You do not have to use this.**

J includes a "nice" web console called `jhs` (`J HTTP Server`) -- unfortunately, while `J` includes a binary for `jhs`, it is somewhat finnicky to work with.
As such, we have created an executable, called `jhs-c` (`c` for "custom" -- so original, right) located in your `/usr/bin` that handles the finnicky stuff.
Just run `jhs-c` in your container to start up the `jhs` server.
Then you can navigate to your browser (default <http://127.0.0.1:65001/jijx>) and enter username `student` with password `password` (ignore any popups from your browser (looking at you, Chrome) that say like "your password has been compromised!" -- duh, obviously don't use `password` as your password -- this is an entirely local server, though, so only _you_ have access to it).
When you want to exit `jhs`, press `ctrl+c` in your container (you may need to press it multiple times), refresh the web browser page, and then your container console should free itself up again (finnicky, I know).

**Again, you do not have to use this.**

### Documentation

Refer to lecture notes -- **read the notes and the available documentation**.

### VSCode

This one provides some syntax highlighting:

[extension](https://marketplace.visualstudio.com/items?itemName=tikkanz.language-j)

## Assignment

J, like MATLAB, is useful for abstract computation.
As such, we aim to implement functions that compute things.
The file `hw.ijs` contains everything you need to get started.
Each function you have to implement has comments describing expected inputs and outputs.

### Testing

Public tests:
```txt
$ jconsole tests-pub.ijs
```

### Evaluation

ez.

* 90% public tests
* 10% no hard-coding, no obvious shortcuts (eg: using `/:~~` for sorting)
