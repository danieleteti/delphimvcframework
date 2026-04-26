# DMVCFramework Entity Generator

Generates `TMVCActiveRecord` entity classes from a database schema.

Available in two versions:
- **MVCEntGen** - Command-line tool (recommended; built on `EntGen.Core.pas`).
- **MVCAREntitiesGenerator** - legacy VCL GUI. Predates `EntGen.Core.pas` and uses its own emission code; new options like `AUTO_REQUIRED` / `AUTO_MAXLENGTH` are CLI-only.

## MVCEntGen (CLI)

### Quick Start

```bash
mvcentgen --config myproject.env --output EntitiesU.pas
```

### Command-Line Options

| Option | Description |
|--------|-------------|
| `--config <file>` | Path to `.env` configuration file (**required**) |
| `--connection <name>` | FireDAC connection definition name (overrides config) |
| `--output <file>` | Output `.pas` file path (overrides config) |
| `--no-auto-required` | Skip `[MVCRequired]` on NOT NULL non-PK columns (default: emit) |
| `--no-auto-maxlength` | Skip `[MVCMaxLength(N)]` on bounded VARCHAR columns (default: emit) |
| `--no-auto-audit` | Skip `[MVCAudit*]` on convention-named audit columns (default: emit) |
| `--no-auto-soft-delete` | Skip `[MVCSoftDeleted]` on convention-named columns (default: emit) |
| `--help` | Show help message |

Both `--option` and `-option` syntax are supported.

### Configuration File

The configuration uses a standard `.env` format (key=value, `#` comments). See `sample_config.env` for a complete example.

#### Database Connection

**Option 1** - Named FireDAC connection definition (from `FDConnectionDefs.ini`):

```env
CONNECTION_DEF=MyConnection
```

**Option 2** - Direct connection parameters:

```env
DRIVER_ID=PG
SERVER=localhost
PORT=5432
DATABASE=mydb
USER_NAME=postgres
PASSWORD=secret
```

Supported drivers: `PG` (PostgreSQL), `FB` (Firebird), `MySQL`, `MSSQL`, `IB` (InterBase), `SQLite`, `Ora` (Oracle), `ODBC`.

Additional FireDAC parameters can be passed with the `FD_` prefix:

```env
FD_CharacterSet=UTF8
FD_OsAuthent=No
```

#### Schema

```env
SCHEMA=public
```

Leave empty for the default schema.

#### Output

```env
OUTPUT_FILE=EntitiesU.pas
```

Can be overridden with `--output`.

#### Generation Options

```env
# MVCNameCase attribute: LowerCase, UpperCase, CamelCase, PascalCase, SnakeCase, AsIs
NAME_CASE=LowerCase

# Property names: AsIs (keep database names) or PascalCase
FIELD_NAME_FORMAT=PascalCase

# Generate ActiveRecordMappingRegistry.AddEntity() calls in initialization section
GENERATE_MAPPING=true

# Generate abstract classes (TCustomXxx instead of TXxx)
CLASS_AS_ABSTRACT=false
```

#### Auto Validation Attributes

The generator infers validation attributes from the database schema. Both flags are **ON by default** in 3.5.0-silicon and later:

| Flag | Default | Effect |
|---|---|---|
| `AUTO_REQUIRED` | `true` | Emit `[MVCRequired]` on every NOT NULL column except auto-generated primary keys. |
| `AUTO_MAXLENGTH` | `true` | Emit `[MVCMaxLength(N)]` on bounded `VARCHAR` / `NVARCHAR` columns. `TEXT` / `CLOB` are skipped. |

Disable per-flag in the config (`AUTO_REQUIRED=false`) or on the command line (`--no-auto-required`, `--no-auto-maxlength`).

When `AUTO_REQUIRED` or `AUTO_MAXLENGTH` is active, the generated unit also imports `MVCFramework.Validators` automatically.

#### Auto-audit columns

When `AUTO_AUDIT` is on (default), columns whose name matches one of four canonical names get the corresponding `[MVCAudit*]` attribute and are excluded from `[MVCRequired]` and `[MVCMaxLength]` — the framework populates these fields in `OnBeforeInsert` / `OnBeforeUpdate`, so the user never writes the value.

| Default name | Attribute emitted | Required Delphi type |
|---|---|---|
| `created_at` | `[MVCAuditCreatedAt]` | `TDateTime` / `NullableTDateTime` |
| `updated_at` | `[MVCAuditUpdatedAt]` | `TDateTime` / `NullableTDateTime` |
| `created_by` | `[MVCAuditCreatedBy]` | `String` / `NullableString` |
| `updated_by` | `[MVCAuditUpdatedBy]` | `String` / `NullableString` |

Override the names if your schema follows a different convention:

```env
AUDIT_CREATED_AT_NAME=row_created
AUDIT_UPDATED_AT_NAME=row_updated
AUDIT_CREATED_BY_NAME=row_created_by
AUDIT_UPDATED_BY_NAME=row_updated_by
```

Disable the whole feature with `--no-auto-audit` or `AUTO_AUDIT=false`.

If a column matches by name but its Delphi type is not the expected one, the audit attribute is **skipped with a warning** — the generator does not emit a wrong attribute for a column that just happens to share the convention name.

#### Auto-soft-delete columns

When `AUTO_SOFT_DELETE` is on (default), columns matching the configured names get `[MVCSoftDeleted]`. Two storage modes:

| Mode | Default names | Required Delphi type |
|---|---|---|
| Timestamp | `deleted_at` | `NullableTDateTime` (the column MUST be NULL-able) |
| Flag | `is_deleted`, `deleted` | `Boolean` |

Both options are CSV lists, override per environment:

```env
SOFT_DELETE_TIMESTAMP_NAME=deleted_at,archived_at
SOFT_DELETE_FLAG_NAME=is_deleted,deleted,is_archived
```

If a table has **both** a timestamp-mode and a flag-mode candidate, the **timestamp wins** (more expressive: also captures *when* the row was soft-deleted) and a warning is logged.

Mismatched types are skipped with a warning. In particular, the framework's timestamp mode requires `NullableTDateTime` — a plain `TDateTime` defaults to `1899-12-30`, which never matches the auto-filter `WHERE deleted_at IS NULL` and would silently break the feature.

The soft-delete column is excluded from `[MVCRequired]` and `[MVCMaxLength]` — the user never writes it directly (use `Delete` / `Restore` / `HardDelete`).

Disable with `--no-auto-soft-delete` or `AUTO_SOFT_DELETE=false`.

#### Read-only and refresh columns

Two comma-separated lists of `table.column` pairs map directly onto the field options emitted next to `[MVCTableField]`:

| Config key | Effect |
|---|---|
| `READONLY_COLUMNS` | Add `foReadOnly`. Use for computed / `GENERATED ALWAYS` / `ROWVERSION` / audit-by-trigger columns the user must not write. `[MVCRequired]` and `[MVCMaxLength]` are also skipped on these fields. |
| `REFRESH_COLUMNS` | Add `foRefresh`. Use for columns whose value should be re-fetched from the DB after `INSERT` / `UPDATE` (e.g. a `BEFORE` trigger normalises the input). |

A column may appear in both lists; the resulting attribute is `[foReadOnly, foRefresh]`. Match is case-insensitive.

```env
READONLY_COLUMNS=customers.full_name,customers.row_ver,products.upper_sku
REFRESH_COLUMNS=orders.normalised_email
```

There is intentionally **no auto-detection**. FireDAC's metadata flags (`caCalculated` / `caReadOnly`) are inconsistently set across drivers — PostgreSQL in particular leaves them unset on `GENERATED ALWAYS` columns — so the generator relies on these explicit lists.

#### Table Filtering

`TABLES` and `EXCLUDE_TABLES` accept a comma-separated list of patterns. Each pattern can be:

| Format | Example | Description |
|--------|---------|-------------|
| Exact name | `customers` | Case-insensitive exact match |
| Wildcard | `TBL_*` | `*` = any characters, `?` = single character |
| Regex | `/^(orders\|items)$/` | Regular expression enclosed in `/` |

Patterns can be mixed freely:

```env
# Only tables starting with TBL_ or VW_, plus the "customers" table
TABLES=TBL_*,VW_*,customers

# Exclude system/temp tables
EXCLUDE_TABLES=__*,tmp_*,/^sys/
```

When `TABLES` is empty (default), all tables are included. `EXCLUDE_TABLES` is applied after `TABLES`.

### Examples

```bash
# Generate from PostgreSQL with direct params
mvcentgen --config pg_project.env

# Use a named connection, override output path
mvcentgen --config myproject.env --connection PROD_DB --output ..\src\EntitiesU.pas

# Generate for specific tables only (configured in .env)
# TABLES=customers,orders,order_items
mvcentgen --config myproject.env
```

### Generated Output

For each selected table, the tool generates:

- A class inheriting from `TMVCActiveRecord` with `[MVCTable]` and `[MVCNameCase]` attributes
- Private fields with `[MVCTableField]` attributes (primary keys, auto-generated flags, JSON/XML type hints)
- Public read/write properties
- Constructor and destructor (for `TStream` fields)
- Nullable types for nullable columns (`NullableString`, `NullableInt32`, etc.)
- Optional `ActiveRecordMappingRegistry.AddEntity()` registration in the initialization section

## Project Structure

```
EntGen.Core.pas                  Core engine (CLI; the GUI predates it)
EntGen.CLIMain.pas               CLI logic (.env parsing, argument handling)
MVCEntGen.dpr                    CLI program
MVCEntGen.dproj                  CLI project file
MVCAREntitiesGenerator.dpr       GUI program
MVCAREntitiesGenerator.dproj     GUI project file
MainFormU.pas / .dfm             GUI form
sample_config.env                Example configuration file
```

## Building

```bash
# CLI
call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
msbuild MVCEntGen.dproj /p:Config=Release /p:Platform=Win32

# GUI
msbuild MVCAREntitiesGenerator.dproj /p:Config=Release /p:Platform=Win32
```

Output goes to `..\bin\`.
