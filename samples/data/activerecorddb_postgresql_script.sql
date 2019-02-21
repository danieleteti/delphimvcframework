-- Drop table

-- DROP TABLE public.articles

CREATE TABLE articles (
	id bigserial NOT NULL,
	description varchar(100) NOT NULL,
	price int4 NOT NULL,
	CONSTRAINT articles_pkey PRIMARY KEY (id)
);

-- Drop table

-- DROP TABLE public.customers

CREATE TABLE customers (
	id bigserial NOT NULL,
	code varchar(20) NULL,
	description varchar(200) NULL,
	city varchar(200) NULL,
	CONSTRAINT customers_pk PRIMARY KEY (id)
);

-- Drop table

-- DROP TABLE public.order_details

CREATE TABLE order_details (
	id bigserial NOT NULL,
	id_order int8 NOT NULL,
	id_article int8 NOT NULL,
	unit_price numeric(18,2) NOT NULL,
	discount int4 NOT NULL DEFAULT 0,
	quantity int4 NOT NULL,
	description varchar(200) NOT NULL,
	total numeric(18,2) NOT NULL,
	CONSTRAINT order_details_pkey PRIMARY KEY (id)
);

-- Drop table

-- DROP TABLE public.orders

CREATE TABLE orders (
	id bigserial NOT NULL,
	id_customer int4 NOT NULL,
	order_date date NOT NULL,
	total numeric(18,4) NULL,
	CONSTRAINT orders_pkey PRIMARY KEY (id)
);

-- Drop table

-- DROP TABLE public.people

CREATE TABLE people (
	id bigserial NOT NULL,
	last_name varchar(100) NOT NULL,
	first_name varchar(100) NOT NULL,
	dob date NULL,
	full_name varchar(80) NULL,
	is_male bool NULL,
	note text NULL,
	photo bytea NULL,
	CONSTRAINT people_pkey PRIMARY KEY (id)
);

create table phones (
  id bigserial primary key,
  phone_number varchar(200) not null,
  number_type varchar(200) not null,  
  dob date,
  id_person bigint not null references people(id)
);

ALTER TABLE orders ADD CONSTRAINT orders_customers_fk FOREIGN KEY (id_customer) REFERENCES customers(id) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE order_details ADD CONSTRAINT order_details_orders_fk FOREIGN KEY (id_order) REFERENCES orders(id) ON DELETE CASCADE ON UPDATE CASCADE;