-- Drop table

-- DROP TABLE public.articles

CREATE TABLE articles (
	id integer NOT NULL,
	description varchar(100) NOT NULL,
	price integer NOT NULL,
	CONSTRAINT articles_pkey PRIMARY KEY (id)
);

-- Drop table

-- DROP TABLE public.customers

CREATE TABLE customers (
	id integer NOT NULL,
	code varchar(20) NOT NULL,
	description varchar(200),
	city varchar(200),
	CONSTRAINT customers_pk PRIMARY KEY (id)
);

-- Drop table

-- DROP TABLE public.order_details

CREATE TABLE order_details (
	id integer NOT NULL,
	id_order integer NOT NULL,
	id_article integer NOT NULL,
	unit_price numeric(18,2) NOT NULL,
	discount integer DEFAULT 0 NOT NULL ,
	quantity integer NOT NULL,
	description varchar(200) NOT NULL,
	total numeric(18,2) NOT NULL,
	CONSTRAINT order_details_pkey PRIMARY KEY (id)
);

-- Drop table

-- DROP TABLE public.orders

CREATE TABLE orders (
	id integer NOT NULL,
	id_customer integer NOT NULL,
	order_date date NOT NULL,
	total numeric(18,4) NOT NULL,
	CONSTRAINT orders_pkey PRIMARY KEY (id)
);

-- Drop table

-- DROP TABLE public.people

CREATE TABLE people (
	id integer NOT NULL,
	last_name varchar(100) NOT NULL,
	first_name varchar(100) NOT NULL,
	dob date NOT NULL,
	full_name varchar(80) NOT NULL,
	is_male BOOLEAN DEFAULT TRUE NOT NULL,
	note  blob sub_type TEXT,
	photo blob sub_type binary,
	CONSTRAINT people_pkey PRIMARY KEY (id)
);




ALTER TABLE orders ADD CONSTRAINT orders_customers_fk FOREIGN KEY (id_customer) REFERENCES customers(id) ON DELETE CASCADE ON UPDATE CASCADE;
ALTER TABLE order_details ADD CONSTRAINT order_details_orders_fk FOREIGN KEY (id_order) REFERENCES orders(id) ON DELETE CASCADE ON UPDATE CASCADE;
