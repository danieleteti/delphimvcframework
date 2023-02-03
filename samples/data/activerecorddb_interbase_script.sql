create database 'C:\DEV\dmvcframework\samples\data\ACTIVERECORDDB.IB' default character set UTF8;
connect 'C:\DEV\dmvcframework\samples\data\ACTIVERECORDDB.IB';

CREATE TABLE articles (
	id integer not null,
	description varchar(100) character set utf8 NOT NULL,
	price integer NOT NULL,
	CONSTRAINT articles_pkey PRIMARY KEY (id)
);

CREATE TABLE customers (
	id integer not null,
	code varchar(20)  character set utf8,
	description varchar(200) character set utf8 ,
	city varchar(200) character set utf8 ,
	rating INTEGER,
	NOTE BLOB SUB_TYPE 1,
	CONSTRAINT customers_pk PRIMARY KEY (id)
);

CREATE TABLE CUSTOMERS_WITH_GUID (
	IDGUID VARCHAR(38) NOT NULL,
	CODE VARCHAR(20),
	DESCRIPTION VARCHAR(200),
	CITY VARCHAR(200),
	NOTE BLOB SUB_TYPE TEXT,
	RATING INTEGER,
	CONSTRAINT CUSTOMERS_WITH_GUID_PK PRIMARY KEY (IDGUID)
);

CREATE TABLE customers_plain (
    id integer NOT NULL,
    code varchar(20) character set utf8 ,
    description varchar(200) character set utf8 ,
    city varchar(200) character set utf8 ,
    note blob sub_type text,
    rating smallint,
    creation_time time,
    creation_date date,
    CONSTRAINT customers_plain_pk PRIMARY KEY (id)
);

CREATE TABLE customers_with_code (
    code varchar(20) character set utf8  NOT null  primary key,
    description varchar(200) character set utf8 ,
    city varchar(200) character set utf8 ,
    NOTE BLOB SUB_TYPE 1,
    rating smallint
);

CREATE TABLE order_details (
	id integer not null,
	id_order integer NOT NULL,
	id_article integer NOT NULL,
	unit_price numeric(18,2) NOT NULL,
	discount integer DEFAULT 0 NOT NULL ,
	quantity integer NOT NULL,
	description varchar(200) character set utf8 NOT NULL ,
	total numeric(18,2) NOT NULL,
	CONSTRAINT order_details_pkey PRIMARY KEY (id)
);

CREATE TABLE orders (
	id integer not null,
	id_customer integer NOT NULL,
	order_date date NOT NULL,
	total numeric(18,4) NOT NULL,
	CONSTRAINT orders_pkey PRIMARY KEY (id)
);


CREATE TABLE people (
	id integer not null,
	last_name varchar(100) character set utf8  NOT NULL ,
	first_name varchar(100) character set utf8 NOT NULL ,
	dob date NOT NULL,
	full_name varchar(80) character set utf8 NOT NULL ,
	is_male BOOLEAN DEFAULT TRUE NOT NULL,
	note  blob sub_type 1,
	photo blob sub_type 0,
	person_type varchar(40),
	salary number(18,4),
	annual_bonus number(18,4),	
	CONSTRAINT people_pkey PRIMARY KEY (id)
);

create table phones (
  id integer not null,
  phone_number varchar(200) character set utf8 not null ,
  number_type varchar(200) character set utf8 not null ,
  dob date,
  id_person integer not null references people(id)
);

CREATE TABLE nullables_test (
	f_int2 smallint ,
	f_int8 integer ,
	f_int4 integer ,
	f_string varchar(200) ,
	f_bool boolean ,
	f_date date ,
	f_time time ,
	f_datetime timestamp,
	f_float4 float,
	f_float8 DOUBLE PRECISION,
	f_currency numeric(18, 4),
	f_blob blob sub_type 0 
);

CREATE TABLE "customers with spaces" (
	"id with spaces" INTEGER NOT NULL,
	"code with spaces" VARCHAR(20),
	"description with spaces" VARCHAR(200),
	"city with spaces" VARCHAR(200),
	"note with spaces" BLOB SUB_TYPE 1,
	"rating with spaces" INTEGER,
	CONSTRAINT cust_with_space_pk PRIMARY KEY ("id with spaces")
);


ALTER TABLE orders ADD CONSTRAINT orders_customers_fk FOREIGN KEY (id_customer) REFERENCES customers(id) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE order_details ADD CONSTRAINT order_details_orders_fk FOREIGN KEY (id_order) REFERENCES orders(id) ON DELETE CASCADE ON UPDATE CASCADE;


CREATE GENERATOR SEQ_ARTICLES_ID;

CREATE GENERATOR SEQ_CUSTOMERS_ID;

CREATE GENERATOR SEQ_ORDER_DETAILS_ID;

CREATE GENERATOR SEQ_ORDERS_ID;

CREATE GENERATOR SEQ_PEOPLE_ID;

CREATE GENERATOR SEQ_PHONES_ID;


/* Triggers only will work for SQL triggers */

CREATE TRIGGER ARTICLES_BI FOR ARTICLES 
ACTIVE BEFORE INSERT POSITION 0
AS BEGIN
  IF (NEW.ID IS NULL) THEN NEW.ID = GEN_ID(SEQ_ARTICLES_ID, 1);
END
 ;


/* Triggers only will work for SQL triggers */

CREATE TRIGGER CUSTOMERS_BI FOR CUSTOMERS 
ACTIVE BEFORE INSERT POSITION 0
AS BEGIN
  IF (NEW.ID IS NULL) THEN NEW.ID = GEN_ID(SEQ_CUSTOMERS_ID, 1);
END
 ;
 
 /* Triggers only will work for SQL triggers */

CREATE TRIGGER ORDERS_BI FOR ORDERS 
ACTIVE BEFORE INSERT POSITION 0
AS BEGIN
  IF (NEW.ID IS NULL) THEN NEW.ID = GEN_ID(SEQ_ORDERS_ID, 1);
END
 ;
 
 /* Triggers only will work for SQL triggers */

CREATE TRIGGER ORDER_DETAILS_BI FOR ORDER_DETAILS 
ACTIVE BEFORE INSERT POSITION 0
AS BEGIN
  IF (NEW.ID IS NULL) THEN NEW.ID = GEN_ID(SEQ_ORDER_DETAILS_ID, 1);
END
 ;
 
 
CREATE TRIGGER PEOPLE_BI FOR PEOPLE 
ACTIVE BEFORE INSERT POSITION 0
AS BEGIN
  IF (NEW.ID IS NULL) THEN NEW.ID = GEN_ID(SEQ_PEOPLE_ID, 1);
END
 ;
 
 CREATE TRIGGER PHONES_BI FOR PHONES 
ACTIVE BEFORE INSERT POSITION 0
AS BEGIN
  IF (NEW.ID IS NULL) THEN NEW.ID = GEN_ID(SEQ_PHONES_ID, 1);
END
 ;
 
 
 
 
 