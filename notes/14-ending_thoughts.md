# Applications

This is it, the last week of class. There is no project this week because of 
the final exam. We may have started out rocky, but I think we ended up finding
a good balance. Hopefully, along the way, you learned some things. I want to 
put in perspective this course and why do taught what we did. 
## Background

If you think about it, all that's going on in a computer is bytes of 1s and 0s
and putting them in a specific order which we all agree means something. In this
class, you learned paradigms, which in my opinion, are basically just schools of
thought on how to think of using, representing and manipulating these bit 
streams. Remember at the end of the day, languages are just ways to express
what we want to do with these bits. At the high level these things are called 
idioms (things that are language independent). 

## Whats the Point

You can now be more expressive with to your computer. You can now be more
concise with your expressions. Consider the following code:
```C
#include <stdio.h>

void* add1(void* a){
	*(int*)a = (*(int*)a + 1);
	return a;
}

void* apply(void * fun (void*), void* a){
	return fun(a);
}

int main(){
	int value = 7;
	apply(add1, &value);
	printf("Value is: %d\n", value);
	return 0;
}
```

and its analogous equivalent in racket:

```racket
	(define (my-apply input-func param) (input-func param));;
	(my-apply add1 7);;
```

Now in the C code, you know have basically functional programming, where you 
can treat functions as data. But as you can see its more concise and easier to
read and use in racket. Now, conversely, you can now introduce concepts and
parts of paradigms to languages you already know, like how we just added 
functional aspects to C. 

If you looked at Befunge's project, you can tell that it has elements of Forth
in it. Rust is basically C except you take out the vulnerabilities and you add
more secure concepts. You can easily get a Rust `slice` with a C `struct`. Lua
which has no tuples, can have something act like a tuple with just a tables. 
However in all these cases, the overhead is immense and you might as well just
switch to whatever language you feel to best compliment the problem you are 
trying to solve which leads to the next point: certain problems are easier to
solve in certain ways. 

## Restraints of language

One thing that really interests me are words in other languages which we do not
have in English. Examples: Komorebi (Japanese): the effect of sunlight shining 
through the leaves of a tree. Zugzwang (German - used in Chess): all moves are
worse than takng your turn. Estrenar (Spanish) - to wear something for the first
time. The interesting thing about this is that when you have a word for 
something, it is hypothesized that you are more sensitive to it. It's like 
its easier to express yourself when you have a word for what you are feeling.

Programming paradigms are just like this, they are more oriented around certain
aspects. Suppose I want give you a list of people who are over 6 feet tall
and a list of people who like the color blue. If i ask you to tell me all the 
people who are over 6 feet and like blue, there's a few ways you could solve
this problem. You could just iterate through the list of one and check if 
that person is on the other list. This is a simple double for loop and 
comparison algorithm in most languages. Yet in sql you could just query the 
lists on people who match both conditions (`select * from people WHERE height>6 
AND color='blue'`). Or in Prolog you could create a relationship  and ask who
is in this relationship (`querry(x) :- blue(x),sixfeet(x).`). Of course this is
a trade off in all of these, the overhead of putting properly formed data into
whatever language you are using, but suppose you didn't have to worry about 
that. I would argue that the double for loop is not the best way to get an 
answer. 

In your other CS classes, you may be taught that there are usually 2 major 
constraints on an algorithm, space and time. I argue that ease of programming
should be another constraint. Think about how fast your programs could be if you
wrote them all in straight assembly. No one does this because writing in 
assembly is hard (though if you take compilers, security or OS, you get used
to it). Which leads to the final point: learning a language is hard. 

## Language Fluency

Learning a language cannot be done in 14 days. However, you can learn how to 
express the basics in a language in 14 days. We were initially too ambitious 
when we believed you can learn the basics in 7 days, but 14 days to learn 
looping, conditionals, and primitives seemed to be fine (along with any other
basics needed to solve most problems). Yet just because you can use a language
does not make one fluent. It takes time to be comfortable with a language, and
to be able to use it to do all that you want. 

However, just like spoken languages, the more languages you learn, the more 
syntaxes and grammars and paradigms you learn, the easier it is to learn more.
You may recognize patterns in languages to help you learn them, and this works
on the meta aspect as well. Languages have patterns and there are patterns to
languages that once you learn the easier it is to learn new ones because you 
are already familiar.

One of the goals of this course was to get you introduced to a variety of 
languages so if you wanted to explore them more you could, or if you needed to
learn a new language it would be easier to do so. If you want to continue from
this point, the next step would to be able to recognize when to use which 
language, or train you to think in different ways to know how to use each 
language in a certain setting. Giving you one project per language would not be
sufficient to do this. We hope that this course was helpful in teaching you
how to use the tools and to introduce new ones to you.

## Conclusion

Languages are weird and learning them takes time. You should understand a 
paradigm to understand when to use it rather than to learn a language to finish
a project.
