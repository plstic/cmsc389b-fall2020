# Introduction/Syllabus

Welcome to **A Tour of Programming Languages**!
We're glad you're here, and we hope to provide you a fun semester filled with interesting programming paradigms and related projects.

For most of these next sections, you can refer directly to the syllabus.
We emphasize some highlights in these notes.

## Course Goals

We have enumerated on the course syllabus a few goals for students taking this course:

* To gain experience in programming language paradigms and design
* To gain a more expansive picture of programming languages, and understand how languages express meaning
* To understand how different programming languages can be used to accomplish the same task
* To mimic features of a programming language in another

But we really have one main goal for this class -- to give you _confidence_ in learning new languages efficiently and using them effectively.

## Class Structure

We will use a handful of services to run this course:
* GitHub -- notes and assignment distribution
* Gradescope -- assignment submission and evaluation
* Canvas -- gradebook 
* Panopto -- video lecture distribution

The course looks as follows:
* New topic every week
* Associated language per topic
* Associated project (almost) per language
   - Assigned on the Friday that we learn the topic
   - Due the following Friday

## Grading

* 66% Assignments (`11 * 6% = 66%`)
* 34% Exams (`2 * 17% = 34%`)
* Some quizzes/surveys for extra credit

The course is set up for you to succeed, so long as you put in the work.

## Assignments 

You may discuss the _assignments_ with your peers, but no copying code!!!!
Additionally, we understand that we are living in the end times, so we have 
granted you two tokens of prolonged life. Each token grants the user 24 hours of
immunity to the Late Penalty debuff.

## Exams

**Not cumulative!**

> Both will be take-home exams featuring a variety of questions, including coding problems more difficult than the weekly assignments. Students will have one week to complete each exam. **Exams are to be done independently**.

## Help

We will post numerous resources per language, our project specs will be well-described, we will have posted lecture videos and notes, and we have office hours.

# Programming Languages and Paradigms

Before we hop into learning actual languages and paradigms, we should learn what 
exactly those terms mean.

## Languages

A **Programming Language** is a language which represents actions, or a set of 
instructions which produce something. Much like how there are various spoken 
languages, there is a wide variety of programming languages. Addionally, we can
have different segments of code which can be analogous to the same programming
idiom. An idiom is a non-language-specific construct such as a data structure, 
algorithm, or series of instructions.
For example:
```Python
print("Hello, World!")
```
```Java
System.out.println("Hello, World!");
```
express the same "Hello World!" program idiom.

In this course we will be introducing you to `>= 10` languages all with different
syntax. 

## Paradigms

A **Programming Paradigm** on the other hand categorizes languages based on the 
features they provide. For the most part, languages are multi-paradigmatic as
many high level languages include a variety of ideas. You have already been 
introduced to some such languages like Java and C. We will be introducing you to
`>= 10` paradigms, some of which you may have seen before and others more esoteric 
in nature. Many languages' syntax come from the paradigms off of which they are
based. Many object-oriented languages, for instance, use the dot notation when 
referring to a class' instance variables.

The [Wikipedia page](https://en.wikipedia.org/wiki/Programming_paradigm) on this 
is a wonderful resource.

## Meta Info

The most important thing to remember is that a programming language is simply a 
tool to solve a problem. The main difference between any programming language is 
_how_ they solve problems. Some problems are easier to solve in certain 
languages than others. As a computer scientist, it is up to you be informed on 
the types of languages available so you know which one to apply to your specific 
problem.

## The Programming Language Learning Efficiency Guide (PL-LEG) (c)

For those that have never heard of the Church-Turing thesis, Alan Turing was a 
wonderful gay man who hypothesised that every solvable problem can be carried out
by a Turing Machine. In ELI5 manner, a Turing machine is a machine that 
consists of an infinitely long tape split into cells. Each cell holds a 1 
or a 0 and the machine reads or writes each cell. This means, that to carry
out a solution to any solvable problem, one only needs to move left, move right,
read and write into a cell. Similarly, to solve many problems in a programming
language, we each have our own lists of what we do to pick up a language.

Cliff's list of success(R)
1. arithmetic
1. control Flow (Conditionals and looping, function calls)
1. memory
1. strings
1. simple IO

Justin's list of success(tm)
1. documentation
1. file/folder structure, simple IO
1. arithmetic, data structures
1. control flow, functions

Ultimately, there is no right way to learn a language, however there are 
inefficient ways. For example, learning pointer arithmetic without first 
learning how to add numbers in C. 

We have spent much time designing this course to be easily digestable, however 
we are ultimtey not the ones in charge of your learning. How you decide to 
approach this course is up to you, and we will do our best to guide you through 
the experience.
