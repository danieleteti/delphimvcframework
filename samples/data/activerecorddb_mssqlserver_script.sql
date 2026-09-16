-- =============================================================================
-- DelphiMVCFramework - ActiveRecord Showcase database (SQL Server)
--
-- Creates the `activerecorddb` database and every table referenced by the
-- samples/activerecord_showcase entities. Run from a context that has CREATE
-- DATABASE rights (sa or equivalent). If the database already exists and you
-- want a clean rebuild, drop it manually before running this script.
--
-- Tested with SQL Server 2019 / 2022 and "ODBC Driver 18 for SQL Server".
-- =============================================================================

IF DB_ID('activerecorddb') IS NULL
    CREATE DATABASE activerecorddb;
GO

USE activerecorddb;
GO

-- -----------------------------------------------------------------------------
-- articles
-- -----------------------------------------------------------------------------
CREATE TABLE articles (
    id          integer IDENTITY(1, 1),
    description nvarchar(100) NOT NULL,
    price       integer       NOT NULL,
    CONSTRAINT articles_pkey PRIMARY KEY (id)
);

-- -----------------------------------------------------------------------------
-- customers
-- -----------------------------------------------------------------------------
CREATE TABLE customers (
    id                      integer IDENTITY(1, 1),
    code                    nvarchar(20),
    description             nvarchar(200),
    city                    nvarchar(200),
    rating                  integer,
    last_contact_timestamp  datetime NULL,
    note                    nvarchar(max),
    CONSTRAINT customers_pk PRIMARY KEY (id)
);

-- -----------------------------------------------------------------------------
-- customers2 (same shape as customers, no PK constraint by design)
-- -----------------------------------------------------------------------------
CREATE TABLE customers2 (
    id                      bigint IDENTITY(1, 1),
    code                    nvarchar(20),
    description             nvarchar(200),
    city                    nvarchar(200),
    note                    nvarchar(max),
    rating                  integer,
    last_contact_timestamp  datetime NULL
);

-- -----------------------------------------------------------------------------
-- customers_plain (client-assigned PK)
-- -----------------------------------------------------------------------------
CREATE TABLE customers_plain (
    id            integer NOT NULL,
    code          nvarchar(20),
    description   nvarchar(200),
    city          nvarchar(200),
    note          nvarchar(max),
    rating        smallint,
    creation_time time,
    creation_date date,
    CONSTRAINT customers_plain_pk PRIMARY KEY (id)
);

-- -----------------------------------------------------------------------------
-- customers_with_code (string PK)
-- -----------------------------------------------------------------------------
CREATE TABLE customers_with_code (
    code        nvarchar(20) NOT NULL PRIMARY KEY,
    description nvarchar(200),
    city        nvarchar(200),
    note        nvarchar(max),
    rating      smallint
);

-- -----------------------------------------------------------------------------
-- customers_with_guid (GUID PK)
-- -----------------------------------------------------------------------------
CREATE TABLE customers_with_guid (
    idguid      uniqueidentifier NOT NULL,
    code        varchar(20)   NULL,
    description varchar(200)  NULL,
    city        varchar(200)  NULL,
    note        text          NULL,
    rating      smallint      NULL,
    CONSTRAINT customers_with_guid_pk PRIMARY KEY (idguid)
);

-- -----------------------------------------------------------------------------
-- customers_with_version (optimistic locking via foVersion)
-- -----------------------------------------------------------------------------
CREATE TABLE customers_with_version (
    id          bigint IDENTITY(1, 1),
    code        varchar(20),
    description varchar(200),
    city        varchar(200),
    note        text,
    rating      integer,
    objversion  integer
);

-- -----------------------------------------------------------------------------
-- customers with spaces (table + column names intentionally contain spaces)
-- -----------------------------------------------------------------------------
CREATE TABLE [customers with spaces] (
    [id with spaces]          bigint IDENTITY(1, 1) NOT NULL,
    [code with spaces]        nvarchar(20),
    [description with spaces] nvarchar(200),
    [city with spaces]        nvarchar(200),
    [rating with spaces]      integer,
    [note with spaces]        nvarchar(max),
    CONSTRAINT [customers with spaces pk] PRIMARY KEY ([id with spaces])
);

-- -----------------------------------------------------------------------------
-- orders / order_details
-- -----------------------------------------------------------------------------
CREATE TABLE orders (
    id          integer IDENTITY(1, 1),
    id_customer integer NOT NULL,
    order_date  date    NOT NULL,
    total       numeric(18, 4) NOT NULL,
    CONSTRAINT orders_pkey PRIMARY KEY (id)
);

CREATE TABLE order_details (
    id          integer IDENTITY(1, 1),
    id_order    integer NOT NULL,
    id_article  integer NOT NULL,
    unit_price  numeric(18, 2) NOT NULL,
    discount    integer DEFAULT 0 NOT NULL,
    quantity    integer NOT NULL,
    description nvarchar(200) NOT NULL,
    total       numeric(18, 2) NOT NULL,
    CONSTRAINT order_details_pkey PRIMARY KEY (id)
);

ALTER TABLE orders
    ADD CONSTRAINT orders_customers_fk FOREIGN KEY (id_customer) REFERENCES customers (id)
        ON DELETE CASCADE ON UPDATE CASCADE;

ALTER TABLE order_details
    ADD CONSTRAINT order_details_orders_fk FOREIGN KEY (id_order) REFERENCES orders (id)
        ON DELETE CASCADE ON UPDATE CASCADE;

-- -----------------------------------------------------------------------------
-- people / phones (inheritance + 1:N relation)
-- -----------------------------------------------------------------------------
CREATE TABLE people (
    id           integer IDENTITY(1, 1),
    last_name    nvarchar(100) NOT NULL,
    first_name   nvarchar(100) NOT NULL,
    dob          date          NOT NULL,
    full_name    nvarchar(80)  NOT NULL,
    is_male      bit           DEFAULT 1 NOT NULL,
    note         nvarchar(max),
    photo        varbinary(max),
    person_type  nvarchar(40)  NOT NULL,
    salary       numeric(18, 4),
    annual_bonus numeric(18, 4),
    CONSTRAINT people_pkey PRIMARY KEY (id)
);

CREATE TABLE phones (
    id            integer IDENTITY(1, 1),
    phone_number  nvarchar(200) NOT NULL,
    number_type   nvarchar(200) NOT NULL,
    dob           date,
    id_person     integer NOT NULL REFERENCES people (id)
);

-- -----------------------------------------------------------------------------
-- nullables_test (one column per NullableXxx type)
-- -----------------------------------------------------------------------------
CREATE TABLE nullables_test (
    f_int2     smallint,
    f_int8     bigint,
    f_int4     integer,
    f_string   nvarchar(200),
    f_bool     bit,
    f_date     date,
    f_time     time,
    f_datetime datetime,
    f_float4   real,
    f_float8   float,
    f_currency numeric(18, 4),
    f_blob     varbinary(max)
);

-- -----------------------------------------------------------------------------
-- default_values_test (used to verify DB-assigned defaults)
-- -----------------------------------------------------------------------------
CREATE TABLE default_values_test (
    f_int2     smallint NOT NULL PRIMARY KEY,
    f_int4     integer,
    f_int8     bigint,
    f_string   nvarchar(200) DEFAULT 'pippo',
    f_bool     bit           DEFAULT 1,
    f_date     date          DEFAULT '2099-12-31',
    f_time     time          DEFAULT '12:00:00',
    f_datetime datetime      DEFAULT '2099-12-31 12:00:00',
    f_float4   real          DEFAULT 1.5,
    f_float8   float         DEFAULT 2.5,
    f_currency numeric(18, 4) DEFAULT 3.1234,
    f_blob     varbinary(max)
);

-- -----------------------------------------------------------------------------
-- integers_as_booleans (demonstrates int<->bool mapping)
-- -----------------------------------------------------------------------------
CREATE TABLE integers_as_booleans (
    id        bigint NOT NULL IDENTITY PRIMARY KEY,
    done_bool bit       NOT NULL,
    done_int  smallint  NOT NULL
);

-- -----------------------------------------------------------------------------
-- booltest (integer-backed boolean column)
-- -----------------------------------------------------------------------------
CREATE TABLE booltest (
    id        integer NOT NULL IDENTITY PRIMARY KEY,
    boolvalue integer
);
GO
