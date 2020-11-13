# Declarative Programming

Up to now, we have _mostly_ covered languages where we control the flow of execution -- imperative languages.
Again, remember that programming languages can implement multiple paradigms.
For example, J is an array programming language yet is imperative and functional.
Prolog is a logic programming language, which is a subset of declarative languages.

Okay but what _is_ a declarative language?
Declarative programming simply means when you **declare** _what_ should happen, without saying _how_ it should happen.
For example, in Prolog you declare rules but you never tell the interpreter/compiler _how_ to check which propositions/predicates are true.
Another example, in SQL you declare what data you want to select (query) but you do not tell the server _how_ to select those data.

One can view declarative languages as a form of _black box_ to perform computations.
You have access to a set of commands, but you do not control how the language performs the actual computation.
We are essentially describing an API.
In-fact, we are describing, abstractly, domain-specific languages.

## Forms

Declarative programming moreso encompasses the following language paradigms:
1. Constraint programming -- specifying constraints to solve combinatorial problems
1. Logic languages -- as we have seen before
1. Mathematical modeling -- giving the computer functions/models to yield visualization output
1. Domain-specific languages -- the most common; small specialized languages

Logic languages are an important subspace of declarative languages.
We have discussed these in detail prior, though.
We continue here discussing domain-specific languages, because they are important as well.

### Domain-Specific Languages

A domain-specific language is a language that applies to a particular domain.
Meaning, if you have a specific problem to solve (a _domain_, or _problem domain_) then a domain-specific language exists to solve it (and only it).
For example, HTML and CSS are domain-specific languages for the user-interface problem space.
Another example, SQL is a domain-specific language in the database query problem space.

On the other end of the problem-solving spectrum lies general-purpose languages.
Examples include Python and C.
This is, of course, just a spectrum.
For example, C can be thought of as a domain-specific language in the operating systems domain.
Yet, C is quite general-purpose.
Thus we can think of this spectrum as a _how specific_ spectrum.

### Data-Oriented Languages

If you are interested, you might also check out data-driven programming.
This is essentially just text-processing -- the idea is to write code that matches some set of data to do some other processing.
Imagine regular expressions, but slightly more.
Examples include AWK, grep, and sed.

## Examples of Declarative Programming Languages

* ECL
* Gremlin
* Lustre
* Prolog
* **SQL** (any variant)
* SequenceL
* Wolfram

## SQLite

### Intro

[Webpage](https://www.sqlite.org/)

So here's the deal.
Databases are just software that manage data on disk (or in memory, if you're sneaky about it).
But it would kind-of suck if you had to remember and enter in address locations of the data you care about.
It would also suck if you wanted to pull out data that all match some sort of conditions.
Enter SQL -- "query" languages.
SQL is for making queries to databases.
These queries yield subsets of the data that match what you care about.
To make a query, you _declare_ what type of data you are looking for, and the database engine finds it for you.

Cool.

SQL stands for Standard Query Language.
Some people pronounce it "sequel", while others pronounce it "s q l".
There is an objectively superior pronounciation.

### Uppercase

A quick word on uppercase keywords.
You may have seen SQL statements before as entirely uppercase (minus table names, etc).
This is almost universally not a requirement.
It sometimes helps with read-ability, but it _usually_ is not necessary.

### SQL vs SQLite vs MySQL vs ...

SQL is the main language, and all SQL software like SQLite, MySQL, Oracle, etc, just implement SQL.
They each have their own small quirks and requirements, but they are, in-essence, just the same.
This lecture explicitly uses SQLite, but the concepts abstract to other database engines.

SQLite differs from other database engines in that it stores all data in a file (or in memory), as opposed to directly on-disk using a low-level B+Tree.
To run SQLite in-memory, just issue `sqlite3`.
To save your database to a file, or re-open a saved database, just run with a/the filename `sqlite3 fname.db`.

### Hello

While declarative, we can still do some basic computation.
For example, we can implement a basic hello world as follows:
```sql
SELECT "Hello, World!";
```
which yields, in the SQLite prompt:
```txt
sqlite> SELECT "Hello, World!";
Hello, World!
sqlite>
```

Cool!
This also works for basic math:
```txt
sqlite> SELECT 5+5;
10
sqlite> SELECT 5+5, 4-1, 6*3;;
10|3|18
sqlite>
```
We can see some specks of array-programming here.
The bars here represent column separators for the returned query data.

Notice also that our statements (queries) end in semicolons.
A statement in SQL is everything up to a semicolon.
Semicolons end statements.
You can have any whitespace you want, so statements can span multiple lines (until the REPL reaches a semicolon).

Database REPLs almost always have non-sql commands.
You might find `.help` and `.tables` useful commands (no semicolon here!).

### Documentation

* <https://www.sqlite.org/index.html>
* <https://sqlite.org/cli.html>
* <https://www.tutorialspoint.com/sqlite/index.htm>
* <https://www.sqlitetutorial.net/>
* <https://www.datacamp.com/community/tutorials/beginners-guide-to-sqlite>

### Basic Queries

Databases have three basic operations:
- insertion
- deletion
- selection

We almost exclusively care about selecting data, because typically we, as data-scientists, work with already-received data.
It might be nice to see how to create data.

SQL works with tables -- essentially 2D arrays.
Each table has a set of column headers, which describe the _type_ of each element in the column.
Data are, thus, stored in rows -- each row of the table constitutes one piece of data.
The table **schema** is simply the description of each row.
Example:
```txt
CREATE TABLE contacts (
    contact_id INTEGER PRIMARY KEY,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    phone TEXT NOT NULL UNIQUE
);

->

creates a "contacts" table with rows:

   contact_id | first_name | last_name | email | phone

with types INTEGER and TEXT
```

We can have more advanced tables, with intra-table interactions.
You can pick these ideas up in practice, though.

SQL languages differ in the data types they can store.
SQLite offers a limited amount of data types.
All-in-all, data are just strings of bits -- you can use this fact to store whatever structure you really want.
Alas, SQLite has the following data types:
- NULL -- none type
- INTEGER -- integers up to 8 bytes
- REAL -- 8-byte floating point numbers
- TEXT -- unlimited-length strings of characters
- BLOB -- **B**inary **L**arge **OB**jects; just bit-strings

#### Insertion

The first key element of a database is insertion.
If you cannot insert/store data, there is no point of a _data_-base.

The following will insert the given VALUES into the table in a new row at the corresponding columns.
```sql
INSERT INTO tbl (column1, column2, ...) VALUES (value1, value2, ...);
```
Example:
```txt
sqlite> CREATE TABLE contacts (
   ...>     contact_id INTEGER PRIMARY KEY,
   ...>     first_name TEXT NOT NULL,
   ...>     last_name TEXT NOT NULL,
   ...>     email TEXT NOT NULL UNIQUE,
   ...>     phone TEXT NOT NULL UNIQUE
   ...> );
sqlite> INSERT INTO contacts (first_name, last_name) VALUES ("justin", "goodman");
Error: NOT NULL constraint failed: contacts.email
sqlite> INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("justin", "goodman", "test@example.net", "nonya");
sqlite>
sqlite> SELECT * FROM contacts;
1|justin|goodman|test@example.net|nonya
sqlite>
sqlite> INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("dank", "meme", "test@example.net", "nonya");
Error: UNIQUE constraint failed: contacts.phone
sqlite> INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("dank", "meme", "test@example.net", "nonyaaaa");
Error: UNIQUE constraint failed: contacts.email
sqlite> INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("dank", "meme", "test@example.com", "nonyaaaa");
sqlite> SELECT * FROM contacts;
1|justin|goodman|test@example.net|nonya
2|dank|meme|test@example.com|nonyaaaa
sqlite>
```
Notice here how the NOT NULL constraint yields an error, instead of allowing the insertion.
Also notice how the UNIQUE constraint yields an error.

There exists an UPDATE command, which will allow you to bulk-set matching data to new values.
There also exists a REPLACE command, which will allow you to select a specific matching row to update its values.
We do not need these for our purposes, though!

#### Deletion

We can delete rows from a table using the DELETE command.
```sql
DELETE FROM tbl WHERE search_conditions;
```
From the example before, we can delete all rows whose first_name is "justin":
```txt
sqlite> DELETE FROM contacts WHERE first_name = "justin";
sqlite> SELECT * FROM contacts;
2|dank|meme|test@example.com|nonyaaaa
sqlite>
```

The `search_conditions` will become apparent when we discuss selection.

We can drop entire tables by issuing:
```sql
DROP TABLE tbl;
```

```txt
sqlite> DROP TABLE contacts;
sqlite> SELECT * FROM contacts;
Error: no such table: contacts
sqlite>
```

There also exists a VACUUM command, which does the following:
- actually removes deleted data
- defragments data
- optimizes data
We do not need this for our purposes.

#### Selection

The meat.
This is the most important part of SQL.
The idea here is we want to select data that match some search criteria.
```sql
SELECT column1, column2, ... FROM tbl;
```
This will pull all rows, restricted to the given columns, from `tbl` and print them out.
You can specify `*` (the star, multiplication symbol) to mean _all columns_.

We can specify other constraints in our query:
```sql
SELECT column1, column2, ...
FROM tbl
WHERE search_conditions
ORDER BY columnX;
```
This query will select the specified column data from all rows in `tbl` where the `search_condition` is true, and will return this data ordered by the specified `columnX`.
Example:
```txt
sqlite> CREATE TABLE contacts (
   ...>     contact_id INTEGER PRIMARY KEY,
   ...>     first_name TEXT NOT NULL,
   ...>     last_name TEXT NOT NULL,
   ...>     email TEXT NOT NULL UNIQUE,
   ...>     phone TEXT NOT NULL UNIQUE
   ...> );
sqlite> INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("justin", "goodman", "test@example.net", "nonya");
sqlite> INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("dank", "meme", "test@example.com", "nonyaaaa");
sqlite> INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("three", "hi", "test@example.org", "44444");
sqlite> INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("YES", "No", "best@example.org", "5");
sqlite> SELECT * FROM contacts;
1|justin|goodman|test@example.net|nonya
2|dank|meme|test@example.com|nonyaaaa
3|three|hi|test@example.org|44444
4|YES|No|best@example.org|5
sqlite> SELECT first_name, last_name FROM contacts WHERE email LIKE "%.org" ORDER BY last_name;
YES|No
three|hi
```
Data are always returned in ascending order.
In this case, uppercase letters are "less-than" lowercase letters.

Search condition types:
- BETWEEN -- `data BETWEEN low_val AND high_val` gives data that the specified column falls within (inclusive) the range.
- IN -- `data IN list` gives data that are contained within the given list. The list can be explicitly stated, or can be a subquery.
- LIKE -- `data LIKE pattern` gives data that match the given pattern. Almost like a regular expression, but more limited. `%` represents wildcard.

Search conditions can be negated using NOT.

See the examples for more.

We also have GROUP BY and HAVING commands -- we will touch on these when applicable.

### Advanced Queries

This section is dedicated to the advanced querying techniques:
- functions
- subqueries
- intersection/union/except
- joins

#### Functions

We have the following functions available to us:
- AVG
- COUNT
- MAX
- MIN
- SUM

Suppose we wish to take the average of a column of queried data.
Then, in our select statement, we can add the following column: `AVG(columnX)`.
This will return a column of data where each cell is the average of `columnX`.
The same idea applies for the other aforementioned functions.

```txt
sqlite> CREATE TABLE domains (
   ...>     domain_id INTEGER PRIMARY KEY,
   ...>     domain_name TEXT NOT NULL,
   ...>     price REAL NOT NULL,
   ...>     owner_email TEXT NOT NULL
   ...> );
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("test.dev", 9.99, "test@example.net");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("best.dev", 9.99, "test@example.net");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("west.dev", 9.99, "test@example.net");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("dank.dev", 9.99, "test@example.com");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("frank.dev", 10.99, "test@example.com");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("stank.dev", 10.99, "test@example.com");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("potato.dev", 4.99, "test@example.org");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("tomato.dev", 4.99, "gest.topic@example.org");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("rotato.dev", 4.99, "gest.topic@example.org");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("beans.dev", 99.99, "google@google.com");
sqlite> INSERT INTO domains (domain_name, price, owner_email) VALUES ("deans.dev", 9.99, "test@example.net");
sqlite> SELECT domain_name, price, AVG(price) FROM domains;
test.dev|9.99|16.99
sqlite> SELECT COUNT(price), SUM(price), AVG(price), MIN(price), MAX(price) FROM domains;
11|186.89|16.99|4.99|99.99
sqlite>
```
You can see here that the query returns a single row, because these functions return a single value.
The first column value is just the first price in the total dataset.

We can GROUP BY to apply the function to multiple disjoint sets.
```txt
sqlite> SELECT price, COUNT(price) FROM domains GROUP BY price;
4.99|3
9.99|5
10.99|2
99.99|1
```

You cannot nest these functions, unfortunately.
Instead, you can use subqueries.

#### Subqueries

Sometimes you might want to make two or more separate queries, and combine them somehow.
Enter _subqueries_.
Subqueries are just nested queries.
They allow you to pick out data, and pick out data using that subset.

We can use a subquery inside a WHERE:
```txt
sqlite> SELECT first_name, last_name, email FROM contacts WHERE email IN (
   ...>     SELECT email FROM domains WHERE price > 7.99
   ...> );
justin|goodman|test@example.net
dank|meme|test@example.com
three|hi|test@example.org
YES|No|best@example.org
Sam|Hank|gest.topic@example.org
Alex|More|google@google.com
sqlite>
```
This query returns the contact information for those who own a domain of cost `> 7.99`.

We can use a subquery inside a FROM:
```txt
sqlite> SELECT AVG(temp.tempColName) FROM (
   ...>     SELECT SUM(price) tempColName FROM domains GROUP BY owner_email
   ...> ) AS temp;
37.378
sqlite>
```
This query calculates the total domain cost for each contact, then averages that result.

We can also do a "correlated subquery" where we perform a subquery for each row in the outer query.
This is inefficient, but sometimes necessary when the inner query depends on the outer query.

#### Sets

Queries give data -- we can think of this as a subset of rows from the original dataset.
As such, we might want to combine resulting data from multiple queries together using set operations.
Enter INTERSECT and UNION.

INTERSECT and UNION take two subqueries, and returns the respective set operation of the two subquery results.
The UNION ALL command will keep duplicates (concatenate the results from both subqueries).
The EXCEPT command will perform set subtraction.
```txt
sqlite> SELECT owner_email FROM domains WHERE price = 9.99
   ...> INTERSECT
   ...> SELECT owner_email FROM domains WHERE price = 10.99;
test@example.com
sqlite> SELECT owner_email FROM domains WHERE price = 9.99
   ...> UNION
   ...> SELECT owner_email FROM domains WHERE price = 10.99;
test@example.com
test@example.net
sqlite> SELECT owner_email FROM domains WHERE price = 9.99
   ...> UNION ALL
   ...> SELECT owner_email FROM domains WHERE price = 10.99;
test@example.net
test@example.net
test@example.net
test@example.com
test@example.net
test@example.com
test@example.com
sqlite> SELECT owner_email FROM domains WHERE price = 9.99
   ...> EXCEPT
   ...> SELECT owner_email FROM domains WHERE price = 10.99;
test@example.net
sqlite>
```

The tables in the subqueries need not be the same, but the subquery results **must** have the same number of columns.

#### Joins

The best for last.
A common database practice is to separate data into multiple tables, but relate table rows using a common key.
For example, we might have one table of customers, and another table of invoices.
Each invoice (row) might contain a customer ID integer, which corresponds to a row in the customers table.
Then we can relate a list of invoice data to a specific customer's contact information.

There exist three SQLite joins:
- INNER JOIN -- just combines columns of correlated data
- LEFT JOIN -- appears to fill NULL values on uncorrelated joined data
- CROSS JOIN -- essentially performs a cross product between the two subqueries

Note that you can reference a specific table column using dot notation.
`contacts.email` would be the email column in the contacts table.
This is helpful when you must reference columns from multiple tables in a single JOIN query.

We will only focus on the INNER JOIN:
```txt
sqlite> SELECT domains.domain_name, contacts.first_name, contacts.last_name
   ...> FROM contacts INNER JOIN domains ON contacts.email = domains.owner_email;
test.dev|justin|goodman
best.dev|justin|goodman
west.dev|justin|goodman
dank.dev|dank|meme
frank.dev|dank|meme
stank.dev|dank|meme
potato.dev|three|hi
tomato.dev|Sam|Hank
rotato.dev|Sam|Hank
beans.dev|Alex|More
deans.dev|justin|goodman
sqlite>
sqlite> SELECT contacts.first_name, contacts.last_name, COUNT(domains.domain_name) tmp
   ...> FROM contacts INNER JOIN domains ON contacts.email = domains.owner_email
   ...> GROUP BY contacts.first_name
   ...> ORDER BY tmp DESC, contacts.first_name ASC;
justin|goodman|4
dank|meme|3
Sam|Hank|2
Alex|More|1
three|hi|1
sqlite>
```
