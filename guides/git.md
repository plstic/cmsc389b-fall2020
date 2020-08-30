# git

So you wanna `git gud` at `git`, eh?

## Installation

### Windows
Go to the main [git download page](https://git-scm.com/downloads) and choose the installer for your OS.
For Windows users, keep in mind that Windows saves files using `CRLF` line-endings, which is different from the standard Unix `LF` line-ending.
If the `git` installer prompts you about line-endings, you might find it useful to "checkout as-is" and "commit Unix-style".
This helps you avoid weird errors, since the Windows `CRLF` line ending is `\r\n` instead of the (probably) expected `\n`.

### *Unix

Use whatever package manager your OS supports to install the `git` package.
+ ex. `sudo apt-get install git`
  + you may need to install `build-tools` from the appstore on macOS

## Usage

We will be using git to distribute any projects, notes, exams and most other
material for this course. You will need to clone the github repository we will
be storing the content in. You can do this by running `git clone 
https://github.com/plstic/cmsc389b-fall2020`. Once cloned, you should then see
a folder titled `cmsc389b-fall2020` in the current directory. `cd` into this 
directory and then, whenever you need to update the repo (because we released a
new project or notes or something), you can run `git pull`. If you have a merge 
conflict, you should run `git stash && git pull`. See the 
[linux_git.mp4](https://umd.instructure.com/courses/1286199/external_tools/28827) 
for a brief video tutorial on how to use git 
for this class.
