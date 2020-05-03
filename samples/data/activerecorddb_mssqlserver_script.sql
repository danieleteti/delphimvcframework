CREATE TABLE articles (
	id integer  IDENTITY(1, 1) ,
	description varchar(100) NOT NULL,
	price integer NOT NULL,
	CONSTRAINT articles_pkey PRIMARY KEY (id)
);

CREATE TABLE customers (
	id integer  IDENTITY(1, 1) ,
	code varchar(20),
	description varchar(200),
	city varchar(200),
	rating INTEGER,	
	NOTE nvarchar(max),	
	CONSTRAINT customers_pk PRIMARY KEY (id)
);

CREATE TABLE customers_plain (
    id integer NOT NULL,
    code varchar(20),
    description varchar(200),
    city varchar(200),
    note nvarchar(max),
    rating smallint,
    CONSTRAINT customers_plain_pk PRIMARY KEY (id)
);

CREATE TABLE customers_with_code (
    code varchar(20) NOT null primary key,
    description varchar(200),
    city varchar(200),
    NOTE nvarchar(max),
    rating smallint
);

CREATE TABLE order_details (
	id integer  IDENTITY(1, 1) ,
	id_order integer NOT NULL,
	id_article integer NOT NULL,
	unit_price numeric(18,2) NOT NULL,
	discount integer DEFAULT 0 NOT NULL ,
	quantity integer NOT NULL,
	description varchar(200) NOT NULL,
	total numeric(18,2) NOT NULL,
	CONSTRAINT order_details_pkey PRIMARY KEY (id)
);

CREATE TABLE orders (
	id integer  IDENTITY(1, 1) ,
	id_customer integer NOT NULL,
	order_date date NOT NULL,
	total numeric(18,4) NOT NULL,
	CONSTRAINT orders_pkey PRIMARY KEY (id)
);


CREATE TABLE people (
	id integer  IDENTITY(1, 1) ,
	last_name varchar(100) NOT NULL,
	first_name varchar(100) NOT NULL,
	dob date NOT NULL,
	full_name varchar(80) NOT NULL,
	is_male bit DEFAULT 1 NOT NULL,
	note  nvarchar(max),
	photo VARBINARY(MAX),
	CONSTRAINT people_pkey PRIMARY KEY (id)
);

create table phones (
  id integer  IDENTITY(1, 1) ,
  phone_number varchar(200) not null,
  number_type varchar(200) not null,  
  dob date,
  id_person integer not null references people(id)
);

ALTER TABLE orders ADD CONSTRAINT orders_customers_fk FOREIGN KEY (id_customer) REFERENCES customers(id) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE order_details ADD CONSTRAINT order_details_orders_fk FOREIGN KEY (id_order) REFERENCES orders(id) ON DELETE CASCADE ON UPDATE CASCADE;
