SELECT "Hello, World!", 5*7, 2-8;

CREATE TABLE contacts (
    contact_id INTEGER PRIMARY KEY,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL UNIQUE,
    phone TEXT NOT NULL UNIQUE
);
INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("justin", "goodman", "test@example.net", "nonya");
INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("dank", "meme", "test@example.com", "nonyaaaa");
INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("three", "hi", "test@example.org", "44444");
INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("YES", "No", "best@example.org", "5");
INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("Sam", "Hank", "gest.topic@example.org", "12345");
INSERT INTO contacts (first_name, last_name, email, phone) VALUES ("Alex", "More", "google@google.com", "443-443-4443");

CREATE TABLE domains (
    domain_id INTEGER PRIMARY KEY,
    domain_name TEXT NOT NULL,
    price REAL NOT NULL,
    owner_email TEXT NOT NULL
);
INSERT INTO domains (domain_name, price, owner_email) VALUES ("test.dev", 9.99, "test@example.net");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("best.dev", 9.99, "test@example.net");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("west.dev", 9.99, "test@example.net");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("dank.dev", 9.99, "test@example.com");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("frank.dev", 10.99, "test@example.com");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("stank.dev", 10.99, "test@example.com");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("potato.dev", 4.99, "test@example.org");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("tomato.dev", 4.99, "gest.topic@example.org");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("rotato.dev", 4.99, "gest.topic@example.org");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("beans.dev", 99.99, "google@google.com");
INSERT INTO domains (domain_name, price, owner_email) VALUES ("deans.dev", 9.99, "test@example.net");



SELECT * FROM contacts;
SELECT * FROM domains;

SELECT first_name, phone FROM contacts WHERE last_name = "hi";
SELECT first_name, phone FROM contacts WHERE last_name IN ("hi", "Hank");
SELECT domain_name, price FROM domains WHERE price BETWEEN 4.99 AND 10.98;
SELECT domain_name, price FROM domains WHERE price > 4.99 ORDER BY price;
SELECT domain_name, price FROM domains WHERE price > 4.99 ORDER BY price DESC, domain_name ASC;



SELECT domain_name, price, AVG(price) FROM domains;
SELECT COUNT(price), SUM(price), AVG(price), MIN(price), MAX(price) FROM domains;
SELECT price, COUNT(price) FROM domains GROUP BY price;


SELECT first_name, last_name, email FROM contacts WHERE email IN (
    SELECT owner_email FROM domains WHERE price > 7.99
);
SELECT AVG(temp.tempColName) FROM (
    SELECT SUM(price) tempColName FROM domains GROUP BY owner_email
) AS temp;



SELECT owner_email FROM domains WHERE price = 9.99
INTERSECT
SELECT owner_email FROM domains WHERE price = 10.99;

SELECT owner_email FROM domains WHERE price = 9.99
UNION
SELECT owner_email FROM domains WHERE price = 10.99;

SELECT owner_email FROM domains WHERE price = 9.99
UNION ALL
SELECT owner_email FROM domains WHERE price = 10.99;

SELECT owner_email FROM domains WHERE price = 9.99
EXCEPT
SELECT owner_email FROM domains WHERE price = 10.99;



SELECT domains.domain_name, contacts.first_name, contacts.last_name
FROM contacts INNER JOIN domains ON contacts.email = domains.owner_email;

SELECT contacts.first_name, contacts.last_name, COUNT(domains.domain_name) tmp
FROM contacts INNER JOIN domains ON contacts.email = domains.owner_email
GROUP BY contacts.first_name
ORDER BY tmp DESC, contacts.first_name ASC;

