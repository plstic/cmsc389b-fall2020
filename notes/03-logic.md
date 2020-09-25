# Logic Language(s)

## A Review  

### Types of Logic

There are various types of logic that exist, and the exact number of how many 
types exist my differ from who you ask, but let's first talk about types which
you are already familliar with. You may know these from language classes, math 
classes, CMSC250. For those who have taken CMSC421/422 or PHIL170/370 or 
similar classes, this may seem familiar to you.

+ Informal  
Informal logic is akin to what English teachers call *logos.* This is what 
philosophers consider to be the study of the real life argument. I like to refer
to it as 'why I'm right.' It is basically just the argument structure a person
would have when arguing with another person. There are 'good' and 'bad' informal
logic. An example of 'Good' informal logic:   
  - Linux is cheaper than Windows because Linus is free  

  Whereas an example of 'Bad' informal logic is:

  - Windows is the best OS because everyone uses it  

 Despite how you may feel about these arguments, both are 'valid' informal
arguments. The difference here is that one is hopefuly more appealing than the
other. A good informal argument relies on *pathos*, *ethos*, and *logos.* 

+ Formal  
Formal logic is probably what mathematicians, philosphers and CS students think
of when you hear the word 'logic.' That or the rapper. Formal logic has a few
types as well, but you are probably familiar with the two majors forms: 
propositional and predicate.

## Propositional Logic

A proposition is any statement that has a truth value. Propositions are either 
a propositional statement or multiple propositional statements linked together
usually by logical operators. In English, logical operators could be the phrases
 like "or", "and", "not", "because." 

*Examples of Propositional sentences*  

  - My name is Cliff and I am awesome
  - I am at least 18 years of age
  - I eat chocolate because I like it

In logic, we usually substitute these phrases with a symbol, like 'p', 'q', 's.'
the above examples could then be translated into propositional logic like so:

  - p ^ q
  - s
  - r -> t

You saw some of this in CMSC250, or any intro logic class.

## Predicate Logic

The thing about propositional logic is that it cannot express much without 
getting supper complicted. Suppose I wanted to say that everyone in this class 
is a hard worker. I would need to have a predicate for each one of you 
(eg. a ^ b ^ c ^ d ...). This is because in propositional logic, a variable 
stands in for a sentence. That is, a singular true or false statement.Granted I 
could just have a single variable 'p' which means everyone in the class is hard 
working, but that tells you nothing. From a programming perspective, this is 
also not optimal. Thus predicate logic was introduced. Predicate logic 
introduced predicates, quantifiers, and functions propsitional logic. You may 
also know predicate logic as first order logic. You should have seen this in 
CMSC250. A predicate symantically acts like a procedure and tells you about a 
relationship between variables. 

Let's consider the argument:
> Daphne and Astoria are sisters.  
If two people are sisters, they share at least one parent.  
Thus, Daphne and Astoria share at least one parent.

In standard propsitional logic, the argument would look something like:  
>P  
 Q -> R  
 --  
 S

This is not valid and we would need to have a bunch of other predicates to make 
it logical. However if we can use predicate logic, we can make the argument like
so:

> sisters(daphne,astoria)  
(\forall x,y \exists z) sisters(x,y) -> parent(z,daphne) ^ parent(z,astoria)
--
(\exists z)parent(z, X) ^ parent(z,Y)

Predicate logic allows 2 additonal things that cannot be found in propositional 
logic:

+ Quantifiers (\forall, \exists)
+ Predicates which denote relationships
  
You may have seen this in CMSC250 or any equivalent logic course. 

## Prolog

Prolog was developed in France by Alain Colmerauer with Philippe Roussel in 
1972, according to Wikipedia. Which is cool I suppose. Give them credit and all 
that. But the purpose of this class is to teach you how to use it.  

We will be using swi-prolog 8.2.1.

### Terms

In prolog, there are these things called _objects_.  An object is just something
to represent something, so it is not like an object is java. The name of any 
object must begin with a lowercase letter. I can be an object so in prolog I 
would be represented as `cliff`. Things like `gold` `pigs` `computers` are all 
objects. You can think of objects as specific entities which could be assigned 
variables. 

The point of prolog is to figure out and solve relationships between objects. I 
don't mean why my ex dumped me, but I mean relationships like are two people 
sisters (`sisters(daphne, astoria)`)? Does John love books 
(`loves(john, books)`)? For those don't know me, I just gradauted with a 
philosphy degree so I could go into detail about what realtionships are and how 
we define them and how to know what one is and such but I won't bore you here. 
Anywho, all relationships like `sisters` or `loves` should also start with a 
lowercase letter. 

Oh course, John could love many things. If we want the list we may want to know 
what would fulfil the predicate `loves(john, X)`. These placeholders are called 
varaibles. Now of course, technically objects are variables meant to refer to 
actual things but we can ignore this as we don't what a metaphysical crisis 
today. These varaibles start with an uppercase letter. That is, `john` is an 
object and `John` is a variable.  Can be confusing, I know. Don't worry, it can 
get even more confusing from here. 

Now given objects, relationships and variables, what can we do? Well in prolog, 
there are three basic operations you can do:

+ delcaring facts 
+ declaring rules
+ asking questions

A fact is something like "Daphne and Astoria" are sisters. We saw one 
representation of this earlier: `sisters(daphne, astoria).`. Technically the 
relationship name and order is arbitrary, but it is useful for use to assign 
meaningful names and orders. For example, `likes(cliff, chocolate).` should mean
that Cliff likes chocolate. It would make no sense to say that chocolate likes 
Cliff. This order is also important when we querry for information and we 
recieve such a proposition back. 

A rule is a declarion that some fact is dependent on something else, whether it 
be a fact or a group of facts. In English, we may use a word like *if* to denote
a rule. We saw this earlier about sisters and the sharing a parent: 
`sisters(X,Y) := parent(Z,X), parent(Z,Y).`.  

### Procedures 

Remember that the point of prolog is to acertain if a question is true or false,
or, find values for variables given that make the predicate true.

`loves(mary,X)` will return all the values that make this predicate true, 
while `loves(mary,john)` will return true is mary loves john. Thus, we have 
built in logical operators when evaluating predicates.

##### operators

There are 7 main operators you will need for this class.  
+ `=` - Used for _if and only if_. Does not evaluate either side of the 
operator. Thus, `?- 9=9.` returns `true.` whereas `?- 7+2=9.` returns `false.`.
+ `is` - Used for arithmetic expressions and will evaulauate _only_ the right 
hand side of the expression. Thus, `7+2 is 9.` is `false.`, but `9 is 7+2.` is 
`true.`.
+ `==` - Used when checking if two things are identical. `X==2` is only true 
should `X` already be assigned to 2.
+ `=:=` will copare arithmetic expressions
+ `\+` - This is basically the NOT operator. `\+ false.` evaluates to `true.`, 
and `false.` evaluates to `true.`. 
+	`,` - when checking for goal satisfaction, this will `and` the goals
+	`;` - when checking for goal satisfaction, this will `or` the goals

There are also a few built in procedures that you can find 
![here](https://www.swi-prolog.org/pldoc/man?section=builtin).

To create a procedure with return value, you need to have the return value be 
included in the procedure definition. Because prolog is basically dealing with 
`true.`s and `false.`s, any return value needs to be part of the querry. Thus,
consider the following add1 method:  
`add1(X,Y) :- Y is X+1.`  
`?- increment(1,Z).` will return `Z = 2.`. This is because prolog will search 
for a value which satisfies the predicate like what we just saw, or prolog will
verfiy the statement put forth like so: `?- increment(1,2).`. This statement 
will return `true.`.

Now not everything you do will be so easy. Sometime we want longer or more 
complicated functions where recursion would be the way to go. To use recursion
we can just have a separate base case defined from the procedure. Check out this
factorial funtion:  
`factorial(0,1).`  
`factorial(N,F) :- N > 0, N1 is N-1, factorial(N1,F1),F is N*F1.`  

### Goal Satisfaction, Backtracking, The Cut

Remember that prolog has 3 basic operations. We have discussed the first two, 
delcaring facts and rules. Finally we get to the most important part of prolog:
asking and answering questions. With swi-prolog you may delcare rules and facts
in a file. In the repl, you ask questions. That is, each statement you put in 
will be attempted to be evaluated. Questions with no variables will be evaluated
as true or false (`sisters(daphne, astoria)`). Questions with variables will be
evaluated and an attempt to find values for the variable which make the 
statement true will be returned (`sisters(X,astoria)`).

Technically whenever you put a statement, it is evaluated as a goal to be 
satisfied. A fact is just an immediately satisfied question and a rule is a 
immediately satisfied conjunction of goals. In order to satisfy a goal, a 
technique known as backtracking occurs. Backtracking is the process of well, 
backtracking to previous statements to see what has been done to satisfy the 
question. 

Despite being called backtracking, prolog will actually start at the beginning 
of the file to look at the history of things said. Additionally, for conjunctive
statements/goals, prolog will look at satisfying the goals from left to right. 
See `backtracking.pl` for more. 

Now, while backtracking is nice, and goal satisfaction is the point of prolog, 
finding all the solutions to a question cannot always be feasible. Sometimes we 
just need to look at one case, or set of cases given certain criteria. Or assume
that the rules and facts are not logically ordered like in backtracking.pl.
Usually when backtracking, things are done in order from left to right. Thus, 
1,3,4,2,3,4,5,3,4 when trying to satisfy `loves(mary,X),loves(john,X).`.

However, there may be times when once something is satisfied, we can keep the 
value which holds the goals up to that point. This is called "The Cut." The cut
works by committing all the bound values it has made up to that point. If you 
take a look at `cut.pl` you will see that we have one rule that will tell you if
someone is in trouble. Someone is in trouble if they have committed a crime as 
was unlucky enough to be caught. However, we can see that the cut operator (`!`)
occurs after committed. That is once `committed(A,Crime)` has been satisfied, 
with a value for A and a value for Crime, then that is the permanent bindings 
"at that level." Thus, once we see that Cliff has committed a clever comeback, 
should unlucky(cliff) fail, there is no reason to have prolog backtrack to 
`committed(cliff,being_too_attractive)`. We can however go back at a higher 
level and test for a different inTrouble value. That is, had we asked 
`inTrouble(X)`, we fist look at `inTrouble(cliff) and once we see 
`committed(cliff, clever_comeback)`, we don't need to check or see  
`committed(cliff,being_too+attractive)`, but we can still check
inTrouble(justin).


#### Things to note

There is really only one built in data structure: lists. A list can be written
as [1,2,3,4,5] and can be matched on [H|T] for head and tail of a list. 
Everything else can be written as a realtionship like `node(child1,child2).`.
