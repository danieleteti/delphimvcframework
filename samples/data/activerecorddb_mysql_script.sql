-- =============================================================================
-- DelphiMVCFramework - ActiveRecord Showcase database (MySQL / MariaDB)
--
-- Creates the `activerecorddb` database and every table referenced by the
-- samples/activerecord_showcase entities.
--
-- Tested with MySQL 8.0+ and MariaDB 10.6+.
-- =============================================================================

CREATE DATABASE IF NOT EXISTS activerecorddb
    CHARACTER SET utf8mb4
    COLLATE utf8mb4_unicode_ci;

USE activerecorddb;

-- -----------------------------------------------------------------------------
-- articles
-- -----------------------------------------------------------------------------
CREATE TABLE articles (
    id          integer NOT NULL AUTO_INCREMENT,
    description varchar(100) NOT NULL,
    price       integer NOT NULL,
    CONSTRAINT articles_pkey PRIMARY KEY (id)
);

-- -----------------------------------------------------------------------------
-- customers
-- -----------------------------------------------------------------------------
CREATE TABLE customers (
    id                      integer NOT NULL AUTO_INCREMENT,
    code                    varchar(20) NULL,
    description             varchar(200),
    city                    varchar(200),
    rating                  integer NULL,
    last_contact_timestamp  datetime NULL,
    note                    text CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT NULL,
    CONSTRAINT customers_pk PRIMARY KEY (id)
);

-- -----------------------------------------------------------------------------
-- customers2 (same shape as customers)
-- -----------------------------------------------------------------------------
CREATE TABLE customers2 (
    id                      bigint NOT NULL AUTO_INCREMENT PRIMARY KEY,
    code                    varchar(20),
    description             varchar(200),
    city                    varchar(200),
    note                    text,
    rating                  integer,
    last_contact_timestamp  datetime NULL
);

-- -----------------------------------------------------------------------------
-- customers_plain (client-assigned PK)
-- -----------------------------------------------------------------------------
CREATE TABLE customers_plain (
    id            integer NOT NULL,
    code          varchar(20),
    description   varchar(200),
    city          varchar(200),
    note          longtext,
    rating        smallint,
    creation_time time NULL,
    creation_date date NULL,
    CONSTRAINT customers_plain_pk PRIMARY KEY (id)
);

-- -----------------------------------------------------------------------------
-- customers_with_code (string PK)
-- -----------------------------------------------------------------------------
CREATE TABLE customers_with_code (
    code        varchar(20) NOT NULL PRIMARY KEY,
    description varchar(200),
    city        varchar(200),
    note        text,
    rating      smallint
);

-- -----------------------------------------------------------------------------
-- customers_with_guid
--   MySQL / MariaDB have no native GUID type. The framework serialises GUIDs
--   in brace-wrapped form "{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}" (38 chars),
--   same as the Firebird / Interbase scripts, so we size the column accordingly.
-- -----------------------------------------------------------------------------
CREATE TABLE customers_with_guid (
    idguid      varchar(38) NOT NULL,
    code        varchar(20)  NULL,
    description varchar(200) NULL,
    city        varchar(200) NULL,
    note        text         NULL,
    rating      int          NULL,
    CONSTRAINT customers_with_guid_pk PRIMARY KEY (idguid)
);

-- -----------------------------------------------------------------------------
-- customers_with_version (optimistic locking via foVersion)
-- -----------------------------------------------------------------------------
CREATE TABLE customers_with_version (
    id          bigint NOT NULL AUTO_INCREMENT PRIMARY KEY,
    code        varchar(20),
    description varchar(200),
    city        varchar(200),
    note        varchar(1000),
    rating      integer,
    objversion  integer
);

-- -----------------------------------------------------------------------------
-- customers with spaces (table + column names intentionally contain spaces)
-- -----------------------------------------------------------------------------
CREATE TABLE `customers with spaces` (
    `id with spaces`          bigint NOT NULL AUTO_INCREMENT,
    `code with spaces`        varchar(20),
    `description with spaces` varchar(200),
    `city with spaces`        varchar(200),
    `rating with spaces`      integer,
    `note with spaces`        text,
    PRIMARY KEY (`id with spaces`)
);

-- -----------------------------------------------------------------------------
-- orders / order_details
-- -----------------------------------------------------------------------------
CREATE TABLE orders (
    id          integer NOT NULL AUTO_INCREMENT,
    id_customer integer NOT NULL,
    order_date  date    NOT NULL,
    total       numeric(18, 4) NOT NULL,
    CONSTRAINT orders_pkey PRIMARY KEY (id)
);

CREATE TABLE order_details (
    id          integer NOT NULL AUTO_INCREMENT,
    id_order    integer NOT NULL,
    id_article  integer NOT NULL,
    unit_price  numeric(18, 2) NOT NULL,
    discount    integer DEFAULT 0 NOT NULL,
    quantity    integer NOT NULL,
    description varchar(200) NOT NULL,
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
    id           integer NOT NULL AUTO_INCREMENT,
    last_name    varchar(100) NOT NULL,
    first_name   varchar(100) NOT NULL,
    dob          date         NOT NULL,
    full_name    varchar(80)  NOT NULL,
    is_male      boolean      DEFAULT TRUE NOT NULL,
    note         text,
    photo        blob,
    person_type  varchar(40),
    salary       numeric(18, 4),
    annual_bonus numeric(18, 4),
    CONSTRAINT people_pkey PRIMARY KEY (id)
);

CREATE TABLE phones (
    id            integer NOT NULL AUTO_INCREMENT PRIMARY KEY,
    phone_number  varchar(200) NOT NULL,
    number_type   varchar(200) NOT NULL,
    dob           date,
    id_person     integer NOT NULL REFERENCES people (id)
);

-- -----------------------------------------------------------------------------
-- nullables_test (one column per NullableXxx type)
-- -----------------------------------------------------------------------------
CREATE TABLE nullables_test (
    f_int2     smallint      NULL,
    f_int8     bigint        NULL,
    f_int4     integer       NULL,
    f_string   varchar(200)  NULL,
    f_bool     boolean       NULL,
    f_date     date          NULL,
    f_time     time          NULL,
    f_datetime datetime      NULL,
    f_float4   float         NULL,
    f_float8   double        NULL,
    f_currency numeric(18, 4) NULL,
    f_blob     blob          NULL
);

-- -----------------------------------------------------------------------------
-- default_values_test (used to verify DB-assigned defaults)
-- -----------------------------------------------------------------------------
CREATE TABLE default_values_test (
    f_int2     smallint NOT NULL PRIMARY KEY,
    f_int4     integer,
    f_int8     bigint,
    f_string   varchar(200) DEFAULT 'pippo',
    f_bool     boolean      DEFAULT TRUE,
    f_date     date         DEFAULT '2099-12-31',
    f_time     time         DEFAULT '12:00:00',
    f_datetime datetime     DEFAULT '2099-12-31 12:00:00',
    f_float4   float        DEFAULT 1.5,
    f_float8   double       DEFAULT 2.5,
    f_currency numeric(18, 4) DEFAULT 3.1234,
    f_blob     blob
);

-- -----------------------------------------------------------------------------
-- complex_types (JSON in MySQL / MariaDB)
-- -----------------------------------------------------------------------------
CREATE TABLE complex_types (
    id         bigint NOT NULL AUTO_INCREMENT PRIMARY KEY,
    json_field json
);

-- -----------------------------------------------------------------------------
-- integers_as_booleans (demonstrates int<->bool mapping)
-- -----------------------------------------------------------------------------
CREATE TABLE integers_as_booleans (
    id        bigint NOT NULL AUTO_INCREMENT PRIMARY KEY,
    done_bool boolean  NOT NULL,
    done_int  smallint NOT NULL
);

-- -----------------------------------------------------------------------------
-- booltest (integer-backed boolean column)
-- -----------------------------------------------------------------------------
CREATE TABLE booltest (
    id        integer NOT NULL AUTO_INCREMENT PRIMARY KEY,
    boolvalue integer
);
