CREATE TABLE articles (
	id integer NOT NULL AUTO_INCREMENT,
	description varchar(100) NOT NULL,
	price integer NOT NULL,
	CONSTRAINT articles_pkey PRIMARY KEY (id)
);


CREATE TABLE nullables_test (
    f_int2 smallint NULL,
    f_int8 integer NULL,
    f_int4 bigint NULL,
    f_string varchar(50) NULL,
    f_bool bool NULL,
    f_date date NULL,
    f_time time NULL,
    f_datetime timestamp NULL,
    f_float4 float NULL,
    f_float8 double NULL,
    f_currency numeric(18,4) NULL,
    f_blob TEXT  NULL
);


CREATE TABLE customers (
	id integer NOT NULL AUTO_INCREMENT,
	code varchar(20) NULL,
	description varchar(200),
	city varchar(200),
	rating INTEGER NULL,	
    note text character set "utf8mb4" collate "utf8mb4_unicode_ci" DEFAULT NULL,	
	CONSTRAINT customers_pk PRIMARY KEY (id)
);


CREATE TABLE customers_plain (
    id integer NOT NULL,
    code varchar(20),
    description varchar(200),
    city varchar(200),
    note longtext,
    rating smallint,
    creation_time time null,
    creation_date date null,    
    CONSTRAINT customers_plain_pk PRIMARY KEY (id)
);

CREATE TABLE customers_with_code (
    code varchar(20) NOT null primary key,
    description varchar(200),
    city varchar(200),
    NOTE text,
    rating smallint
);


CREATE TABLE customers_with_guid (
	idguid binary(16) NOT NULL,
	code varchar(20) NULL,
	description varchar(200) NULL,
	city varchar(200) NULL,
	note text NULL,
	rating int4 NULL,
	CONSTRAINT customers_with_guid_pk PRIMARY KEY (idguid)
);


CREATE TABLE order_details (
	id integer NOT NULL AUTO_INCREMENT,
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
	id integer NOT NULL AUTO_INCREMENT,
	id_customer integer NOT NULL,
	order_date date NOT NULL,
	total numeric(18,4) NOT NULL,
	CONSTRAINT orders_pkey PRIMARY KEY (id)
);


CREATE TABLE people (
	id integer NOT NULL AUTO_INCREMENT,
	last_name varchar(100) NOT NULL,
	first_name varchar(100) NOT NULL,
	dob date NOT NULL,
	full_name varchar(80) NOT NULL,
	is_male BOOLEAN DEFAULT TRUE NOT NULL,
	note  TEXT,
	photo BLOB,
	person_type varchar(40),
	salary numeric(18,4),
	annual_bonus numeric(18,4),	
	CONSTRAINT people_pkey PRIMARY KEY (id)
);

create table phones (
  id integer NOT NULL auto_increment primary key,
  phone_number varchar(200) not null,
  number_type varchar(200) not null,  
  dob date,
  id_person integer not null references people(id)
);

ALTER TABLE orders ADD CONSTRAINT orders_customers_fk FOREIGN KEY (id_customer) REFERENCES customers(id) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE order_details ADD CONSTRAINT order_details_orders_fk FOREIGN KEY (id_order) REFERENCES orders(id) ON DELETE CASCADE ON UPDATE CASCADE;
