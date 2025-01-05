-- SQlite
CREATE TABLE books (
	id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
	book_name TEXT NOT NULL,
	author_name TEXT NOT NULL,
	genre TEXT NOT NULL, 
	rating INTEGER DEFAULT (0) NOT NULL);
	
	
/* fill data and then generate rating using the following update */

update books set rating = abs(random() % 6)	