# Lua

## Preliminaries

We will be using the standard `Lua` interpreter.
We are using version `5.3.5`.
To check, you can run `lua -v`:
```txt
[student@ed1a7c32a468 ~]$ lua -v
Lua 5.3.5  Copyright (C) 1994-2018 Lua.org, PUC-Rio
```

We are also using LuaRocks version `3.1.1`.
The path to your LuaRocks may look different if not using our Docker image:
```txt
[student@ed1a7c32a468 ~]$ luarocks --version
/usr/local/bin/luarocks 3.3.1
LuaRocks main command-line interface
```
And we are using the `lcurses` rock:
```txt
[student@ed1a7c32a468 ~]$ luarocks list

Rocks installed for Lua 5.3
---------------------------

lcurses
   9.0.0-1 (installed) - /usr/local/lib/luarocks/rocks-5.3
```
Again, your rock install location may be different if not using our Docker image.
Note that `lcurses` is imported like so: `require('curses')` (**not** `require('lcurses')`, somewhat counter-intuitive).

If you're using our Docker image, make sure a `.bashrc` exists in your home directory.
If it does not, you may need to rebuild your image since our provided `Dockerfile` creates a `.bashrc` for you.
Or, if your shared folder is the same as `/home/student` then you'll be overwriting the original contents (and thus missing `.bashrc`).
The specific command we use to ensure LuaRocks works is: `luarocks path >> /home/student/.bashrc`.

This assignment is due **Friday, September 25th at 11:59pm on Gradescope**.
You will submit one `hello.lua` and one `snake.cob` to the `Lua` Gradescope assignment.
You will be graded on public and private tests.

### Documentation

* <https://www.lua.org/> (main page)

Refer to the lecture notes for more.

### VSCode

[extension](https://marketplace.visualstudio.com/items?itemName=trixnz.vscode-lua)

You'll need to change the default `Lua` version to `5.3`:
1. Open the command palette (`ctrl+shift+p`)
1. `Preferences: Open Settings (JSON)`
1. Add in: `"lua.targetVersion": "5.3"` as one of the `json` entries
   * you may need to add a `,` (comma) to the end of the last entry

We test you using version `5.3`, but the `5.3` interpreter should be backwards-compatible to `5.1` and `5.2` (probably).
`Lua` version `5.3` adds in the `//` operator for integer division.

## Assignment

We're implementing Snake.
That's it, that's the whole project.

### Hello

Make `hello.lua` print out "Hello, World!".
Don't over-complicate it lmao.

### Snake

The cool stuff.
Look, Lua's main purpose is to be embedded as scripting inside other systems.
That doesn't mean we can't exploit the language to make low-quality terminal games though.
We're going to make Snake.

In Snake, you are a snake.
You are always moving.
You can control which direction you're going, but your entire snake body follows the exact path you travel.
Fruit is placed on the game board -- when you eat a fruit then your snake body grows by one link.
Your objective is to become as large as possible.
Historically, you'd do this in the least amount of time.

Snake is typically implemented using a linked-list (afaik) to represent the snake.
You keep a pointer to the snake head and tail, and advance/remove those on the game board to create the "snake path travel" effect thing.
This is how our implementation works.

### Implementation Details

Everything is given to you in the actual code.
`hello.lua` tells you exactly what to print.
`snake.lua` includes a lot of comments that explain exactly what to put.
All code for Snake should replace the `-- *** --` comments.
You should not overwrite the starter code, as almost all variables are used to maintain the game state and can/will be tested.
If you think you can implement Snake better than how I've laid it out for you -- I'd love to see it :)

In my opinion, the most complicated part of this project is the `Game.Fruit.init` function, and potentially just understanding the Snake linked structure.
If you're confused, let us know.

### Testing Hints

I get it -- print-testing is popular.
I also use print statements to see where my code is during runtime.
Unfortuantely, interactive mode uses `lcurses` which shadows the current console.
Thus, normal print statements won't work.

If your code is randomly exiting with no output, try running like this:
```txt
lua snake.lua > output.txt
```
This may nudge it to output the assertion error.
The whole point of the `xpcall` wrapper is to facilitate this, but alas Lua is weird.

If your code is playing snake, but you want to see information, you can try the following right before the `stdscr:refresh()`:
```lua
stdscr:mvaddstr(0,0,"string here")
```
which displays `string here` at the top-left of your console.
Beware, though, that any characters you print on the screen could interfere with the game!
The game is written in a way such that it's not a huge concern though.

### Evaluation

Simple.

* 50% Public Tests (you'll receive the tests)
* 50% Private Tests (you'll see the test names, and whether you passed)

You can run the provided public tests by executing:
```txt
lua tests-pub.lua
```
Note that if you fail a specific test, then the script will stop executing and no further tests will be run.

Want some extra credit?
1. figure out why this specific implementation of Snake is broken
1. fix it, and slide it into my DMs
(note that if you follow the project spec, you'll pass all the tests no problem -- the tests are inclusive of the small broken-ness)
