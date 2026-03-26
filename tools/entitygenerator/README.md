# DMVCFramework Entity Generator

Generates `TMVCActiveRecord` entity classes from a database schema.

Available in two versions:
- **MVCAREntitiesGenerator** - GUI application (VCL)
- **MVCEntGen** - Command-line tool

Both share the same core engine (`EntGen.Core.pas`).

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

Supported drivers: `PG` (PostgreSQL), `FB` (Firebird), `MySQL`, `MSSQL`, `IB` (InterBase), `SQLite`, `ODBC`.

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
EntGen.Core.pas                  Core engine (shared between GUI and CLI)
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
